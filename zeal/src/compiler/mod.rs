use anyhow::bail;
use chunk::Chunk;
use opcode::{Op, OpParam, Opcode, VarOp};

use crate::{
    ast::{
        expr::{Expr, ExprList, FormExpr, LoopExpr},
        Ast, BinaryOpType, VarType,
    },
    core_types::{idents, num::ZBool, val::ZValue},
};

pub mod chunk;
pub mod opcode;
pub mod state;

/// Bytecode compiler for ZealVM bytecode
pub struct Archon;

impl Archon {
    pub fn compile(ast: &Ast) -> anyhow::Result<Chunk> {
        // println!("{ast}");
        let mut ch = Chunk::default();
        Self::compile_with(ast, &mut ch)?;

        println!("{ch}");
        Ok(ch)
    }

    pub fn compile_with(ast: &Ast, ch: &mut Chunk) -> anyhow::Result<()> {
        Self::compile_expr(ast, ch, &ast.tree)?;
        Ok(())
    }

    fn compile_logical_op(
        ast: &Ast,
        ch: &mut Chunk,
        operator: (BinaryOpType, &Expr, &Expr),
    ) -> anyhow::Result<()> {
        let (ty, left, right) = operator;
        let op = match ty {
            BinaryOpType::And => Op::JumpFalse,
            BinaryOpType::Or => Op::JumpTrue,
            _ => bail!(
                "compile_logical_op: Tried to compile logical operator but was given type: {ty}"
            ),
        };

        Self::compile_expr(ast, ch, left)?;
        let jump = ch.push_jump(op);
        ch.push_popn(1);
        Self::compile_expr(ast, ch, right)?;
        ch.patch_jump(jump);
        Ok(())
    }

    fn compile_expr_list(ast: &Ast, ch: &mut Chunk, el: &ExprList) -> anyhow::Result<()> {
        match el {
            ExprList::Block(bl) => {
                ch.scope.start_scope();
                for e in bl.iter() {
                    Self::compile_expr(ast, ch, e)?;
                }
                let pops = ch.scope.end_scope();
                if pops > 0 {
                    ch.push_popn(pops as u8);
                }
            }
            ExprList::Tuple(tup) => {
                for ex in tup.iter() {
                    Self::compile_expr(ast, ch, ex)?;
                }
            }
            // ExprList::Unit(ex) => Self::compile_expr(ast, ch, ex.as_ref())?,
            ExprList::Nil => Self::compile_expr(ast, ch, &Expr::Nil)?,
        };
        Ok(())
    }

    fn compile_expr(ast: &Ast, ch: &mut Chunk, expr: &Expr) -> anyhow::Result<()> {
        match expr {
            Expr::Form(form) => {
                match form {
                    FormExpr::When { conds } => {
                        let mut end_patches = Vec::new();
                        for ce in conds.iter() {
                            match ce.as_tup() {
                                (&Expr::Nil, block) => {
                                    Self::compile_expr(ast, ch, block)?;
                                    for patch in end_patches.iter() {
                                        ch.patch_jump(*patch);
                                    }
                                    break;
                                }
                                (cond, block) => {
                                    Self::compile_expr(ast, ch, cond)?;
                                    let cond_jump = ch.push_jump(Op::JumpFalse);
                                    ch.push_popn(1);
                                    Self::compile_expr(ast, ch, block)?;

                                    {
                                        let end_jump = ch.push_jump(Op::Jump);
                                        end_patches.push(end_jump);
                                    }

                                    ch.patch_jump(cond_jump);
                                    ch.push_popn(1);
                                }
                            }
                        }
                        // Self::compile_expr(ast, ch, cond)?;
                        //
                        // let then_patch = ch.push_jump(Op::JumpFalse);
                        // ch.push_popn(1);
                        // Self::compile_expr_list(ast, ch, then_body)?;
                        // let else_jump = ch.push_jump(Op::Jump);
                        // ch.patch_jump(then_patch);
                        // ch.push_popn(1);
                        //
                        // // ch.patch_jump(then_patch);
                        //
                        // if let Some(eb) = else_body {
                        //     match eb.as_ref() {
                        //     // else
                        //     Expr::List(ExprList::Block(bl)) => {
                        //         Self::compile_expr_list(ast, ch, &ExprList::Block(bl.clone()))?;
                        //     }
                        //     // elseif
                        //     Expr::Form(FormExpr::Cond { .. }) => Self::compile_expr(ast, ch, eb.as_ref())?,
                        //     _ => bail!("CompileError => Expected Expr::List(ExprList::Block()) | Expr::Call(CallExpr::If {{..}}) in Archon::compile_expr for else body!!! Probably missing ending else/end/elseif!!!"),
                        // }
                        // }
                        // ch.patch_jump(else_jump);
                    }
                    FormExpr::Loop {
                        body: ExprList::Block(bl),
                        meta,
                    } => {
                        let mut breaks = Vec::new();
                        match meta {
                            LoopExpr::Loop => {
                                ch.scope.start_scope();
                                let top = ch.len() - 1;
                                for ex in bl.iter() {
                                    if matches!(ex, Expr::Form(FormExpr::Break { .. })) {
                                        let jump = ch.push_jump(Op::Jump);
                                        breaks.push(jump);
                                    } else {
                                        Self::compile_expr(ast, ch, ex)?;
                                    }
                                }
                                let jump_op = Opcode::WithParam {
                                    op: Op::Jump,
                                    param: OpParam::byte16(top as u16),
                                };
                                ch.push_opcode(jump_op);
                                let pops = ch.scope.end_scope();
                                for b in breaks {
                                    ch.patch_jump(b);
                                }
                                if pops > 0 {
                                    ch.push_popn(pops as u8);
                                }
                            }
                            LoopExpr::While { cond } => {
                                ch.scope.start_scope();
                                let top = ch.len() - 1;
                                for ex in bl.iter() {
                                    if matches!(ex, Expr::Form(FormExpr::Break { .. })) {
                                        let jump = ch.push_jump(Op::Jump);
                                        breaks.push(jump);
                                    } else {
                                        Self::compile_expr(ast, ch, ex)?;
                                    }
                                }
                                Self::compile_expr(ast, ch, cond)?;
                                let jump_op = Opcode::WithParam {
                                    op: Op::JumpTrue,
                                    param: OpParam::byte16(top as u16),
                                };
                                ch.push_opcode(jump_op);
                                let pops = ch.scope.end_scope();
                                for b in breaks {
                                    ch.patch_jump(b);
                                }
                                if pops > 0 {
                                    ch.push_popn(pops as u8);
                                }
                            }
                            LoopExpr::Each {
                                iter_ident,
                                iterable,
                            } => todo!(),
                            LoopExpr::For { iter_ident, range } => todo!(),
                        }
                    }
                    FormExpr::Loop { .. } => {
                        bail!(
                            "expected block after loop construct ('loop', 'while', 'each', 'for')"
                        )
                    }
                    FormExpr::Match {} => todo!(),
                    FormExpr::Struct {} => todo!(),
                    FormExpr::Impl {} => todo!(),
                    FormExpr::Func {
                        name,
                        arity,
                        body,
                        last,
                    } => todo!(),
                    FormExpr::Print(pex) => {
                        Self::compile_expr(ast, ch, pex)?;
                        ch.push_opcode(Opcode::new(Op::Print));
                    }
                    FormExpr::Println(pex) => {
                        Self::compile_expr(ast, ch, pex)?;
                        ch.push_opcode(Opcode::new(Op::Println));
                    }

                    FormExpr::BinaryOperator {
                        op: op,
                        left: left,
                        right: right,
                    } => match op {
                        BinaryOpType::Gt
                        | BinaryOpType::Lt
                        | BinaryOpType::Ge
                        | BinaryOpType::Le
                        | BinaryOpType::Equals
                        | BinaryOpType::NotEquals
                        | BinaryOpType::BitAnd
                        | BinaryOpType::BitOr
                        | BinaryOpType::Xor
                        | BinaryOpType::Concat
                        | BinaryOpType::Add
                        | BinaryOpType::Sub
                        | BinaryOpType::Mul
                        | BinaryOpType::Div => {
                            Self::compile_expr(ast, ch, left)?;
                            Self::compile_expr(ast, ch, right)?;

                            let op = Op::from(*op);
                            ch.push_opcode(Opcode::new(op));
                        }

                        BinaryOpType::And | BinaryOpType::Or => {
                            Self::compile_logical_op(ast, ch, (*op, left, right))?
                        }
                    },

                    FormExpr::UnaryOperator { op, scalar, .. } => {
                        let op = Op::from(*op);
                        Self::compile_expr(ast, ch, scalar)?;
                        ch.push_opcode(Opcode::new(op));
                    }
                    FormExpr::Assign { binding, rhs } => {
                        let ident = binding;

                        let ty = ast.symbols.var_type(ident.clone());
                        if let Some(VarType::Var) = ty {
                            Self::compile_expr(ast, ch, rhs.as_ref())?;
                            ch.push_binding(ident.clone(), VarOp::Set)?;
                        } else {
                            bail!("Cannot assign to a binding that was not declared as 'var'. ident: {}, var_type: {ty:?}", ident.name());
                        }
                    }
                    FormExpr::Call { head, params } => {
                        todo!()
                        // let head = head.expect_ident();
                        //
                        // if let Some(op) = head.binary_operator() {
                        //     // NOTE: For now mathematic operators have only 2 operands. In the future
                        //     // we can easily allow for a lisp style ops
                        //     let left = &params[0];
                        //     let right = &params[1];
                        //     Self::compile_expr(ast, ch, left)?;
                        //     Self::compile_expr(ast, ch, right)?;
                        //     ch.push_opcode(Opcode::new(op));
                        // } else if let Some(op) = head.unary_operator() {
                        //     let operand = &params[0];
                        //     Self::compile_expr(ast, ch, operand)?;
                        // } else {
                        //     match head.name() {
                        //         idents::ASSIGN => {
                        //             let name = &params[0];
                        //             let ident = name.inner_ident().expect(
                        //                 "first param of Assignment call must be an Identifier!!!",
                        //             );
                        //             let ty = ast.symbols.var_type(&ident);
                        //             if let Some(VarType::Var) = ty {
                        //                 let val = &params[1];
                        //                 Self::compile_expr(ast, ch, val)?;
                        //                 let _ = ch.push_binding(ident.clone(), VarOp::Set)?;
                        //             } else {
                        //                 bail!("Cannot assign to a binding that was not declared as 'var'. ident: {}, var_type: {ty:?}", ident.name());
                        //             }
                        //         }
                        //         idents::PRINT => {
                        //             let right = &params[0];
                        //             Self::compile_expr(ast, ch, right)?;
                        //             ch.push_opcode(Opcode::new(Op::Print));
                        //         }
                        //         idents::PRINTLN => {
                        //             let right = &params[0];
                        //             Self::compile_expr(ast, ch, right)?;
                        //             ch.push_opcode(Opcode::new(Op::Println));
                        //         }
                        //         _ => bail!("cant compile: {head}"),
                        //     };
                        // }
                    }
                    _ => bail!("Unspecified error in compile_expr matching CallExpr::Std(StdForm)"),
                }
            }

            Expr::Binding { ty, name, init } => {
                Self::compile_expr(ast, ch, init)?;
                let name = name.expect_ident();
                ch.declare_binding(name.clone())?;
            }
            Expr::List(li) => Self::compile_expr_list(ast, ch, li)?,
            Expr::Atom(a) => match a {
                ZValue::Nil => {
                    let _ = ch.push_opcode(Opcode::new(Op::Nil));
                }
                ZValue::Bool(ZBool(true)) => {
                    let _ = ch.push_opcode(Opcode::new(Op::True));
                }
                ZValue::Bool(ZBool(false)) => {
                    let _ = ch.push_opcode(Opcode::new(Op::False));
                }
                ZValue::Number(n) => {
                    let _ = ch.push_number(n.unwrap());
                }

                ZValue::Byte(_) => todo!(),
                // ZValue::Buffer(_) => todo!(),
                ZValue::Str(s) => {
                    let _ = ch.push_string(s.as_str());
                }
                ZValue::Vec(v) => {
                    let vec = v.as_ref();
                }
                // ZValue::Obj(_) => todo!(),
                // ZValue::MutRef(_) => todo!(),
                ZValue::Rune(_) => todo!(),
                ZValue::Ident(ident) => {
                    ch.push_binding(ident.clone(), VarOp::Get)?;
                }
                ZValue::Unit => todo!(),
            },

            Expr::Nil => {
                ch.push_opcode(Opcode::new(Op::Nil));
            }
        }
        Ok(())
    }
}
