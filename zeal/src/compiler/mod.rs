use std::rc::Rc;

use anyhow::bail;
use chunk::Chunk;
use opcode::{Op, OpParam, Opcode, VarOp};
use unit::FuncChunk;

use crate::{
    ast::{
        expr::{Expr, ExprList, FormExpr, FuncDecl, FuncExpr, LoopExpr, Namespace, WhenForm},
        Ast, BinaryOpType, VarType,
    },
    core_types::{idents, num::ZBool, val::ZValue},
};

pub mod chunk;
pub mod opcode;
pub mod state;
pub mod unit;

/// Bytecode compiler for ZealVM bytecode
pub struct Archon;

impl Archon {
    pub fn compile(ast: &Ast) -> anyhow::Result<FuncChunk> {
        let ch = Self::compile_to_chunk(ast)?;

        let f = FuncChunk::script(ch);
        Ok(f)
    }

    pub fn compile_func(form: &FuncDecl) -> anyhow::Result<FuncChunk> {
        let mut current = FuncChunk::empty_with_sig(form.name.clone(), form.arity() as u8);

        let chunk = &mut current.chunk;
        chunk.scope.start_scope();

        chunk.declare_local(form.name.clone())?;

        if let Some(args) = form.params.as_ref() {
            for arg in args.iter() {
                chunk.declare_local(arg.clone())?;
            }
        }

        Self::compile_expr_list(chunk, &form.body)?;
        Ok(current)
    }

    pub fn compile_to_chunk(ast: &Ast) -> anyhow::Result<Chunk> {
        let mut ch = Chunk::default();
        Self::compile_expr(&mut ch, &ast.tree)?;
        Ok(ch)
    }

    fn compile_logical_op(
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

        Self::compile_expr(ch, left)?;
        let jump = ch.push_jump(op);
        ch.push_popn(1);
        Self::compile_expr(ch, right)?;
        ch.patch_jump(jump);
        Ok(())
    }

    pub(crate) fn compile_expr_list(ch: &mut Chunk, el: &ExprList) -> anyhow::Result<()> {
        match el {
            ExprList::Block(bl) => {
                ch.scope.start_scope();
                for e in bl.iter() {
                    Self::compile_expr(ch, e)?;
                }
                let pops = ch.scope.end_scope();
                if pops > 0 {
                    ch.push_popn(pops as u8);
                }
            }
            ExprList::Tuple(tup) => {
                for ex in tup.iter() {
                    Self::compile_expr(ch, ex)?;
                }
            }
            // ExprList::Unit(ex) => Self::compile_expr( ch, ex.as_ref())?,
            ExprList::Nil => Self::compile_expr(ch, &Expr::Nil)?,
        };
        Ok(())
    }

    pub(crate) fn compile_expr(ch: &mut Chunk, expr: &Expr) -> anyhow::Result<()> {
        match expr {
            Expr::Form(form) => {
                match form {
                    FormExpr::When { conds } => {
                        let mut end_patches = Vec::new();
                        for ce in conds.iter() {
                            match ce {
                                WhenForm::Branch(cond, block) => {
                                    Self::compile_expr(ch, cond)?;
                                    let cond_jump = ch.push_jump(Op::JumpFalse);
                                    ch.push_popn(1);
                                    Self::compile_expr(ch, block)?;

                                    {
                                        let end_jump = ch.push_jump(Op::Jump);
                                        end_patches.push(end_jump);
                                    }

                                    ch.patch_jump(cond_jump);
                                    ch.push_popn(1);
                                }
                                WhenForm::Else(block) => {
                                    println!("Compiling Else BLock: {block}, {}", block.type_str());
                                    Self::compile_expr(ch, block)?;
                                    break;
                                }
                                WhenForm::End => break,
                            }
                        }
                        for patch in end_patches.iter() {
                            ch.patch_jump(*patch);
                        }
                    }
                    FormExpr::Loop {
                        body: ExprList::Block(bl),
                        meta,
                    } => {
                        let mut breaks = Vec::new();
                        match meta {
                            LoopExpr::Loop => {
                                ch.scope.start_scope();
                                let top = ch.len();
                                for ex in bl.iter() {
                                    if matches!(ex, Expr::Form(FormExpr::Break { .. })) {
                                        let jump = ch.push_jump(Op::Jump);
                                        breaks.push(jump);
                                    } else {
                                        Self::compile_expr(ch, ex)?;
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
                                let top = ch.len();
                                for ex in bl.iter() {
                                    if matches!(ex, Expr::Form(FormExpr::Break { .. })) {
                                        let jump = ch.push_jump(Op::Jump);
                                        breaks.push(jump);
                                    } else {
                                        Self::compile_expr(ch, ex)?;
                                    }
                                }
                                Self::compile_expr(ch, cond)?;
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
                    FormExpr::Proto(ff) => {
                        let child = Self::compile_func(ff)?;
                        ch.push_constant(ZValue::Func(Rc::new(child)));
                    }
                    FormExpr::Print(pex) => {
                        Self::compile_expr(ch, pex)?;
                        ch.push_opcode(Opcode::new(Op::Print));
                    }
                    FormExpr::Println(pex) => {
                        Self::compile_expr(ch, pex)?;
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
                            Self::compile_expr(ch, left)?;
                            Self::compile_expr(ch, right)?;

                            let op = Op::from(*op);
                            ch.push_opcode(Opcode::new(op));
                        }

                        BinaryOpType::And | BinaryOpType::Or => {
                            Self::compile_logical_op(ch, (*op, left, right))?
                        }
                    },

                    FormExpr::UnaryOperator { op, scalar, .. } => {
                        let op = Op::from(*op);
                        Self::compile_expr(ch, scalar)?;
                        ch.push_opcode(Opcode::new(op));
                    }
                    FormExpr::Assign { binding, rhs } => {
                        let ident = binding;

                        // TODO: Dont remove comments beneath these two lines,
                        // im moving the symbol table into the AST nodes, but in case that
                        // never happens, this can be the source of future bugs
                        Self::compile_expr(ch, rhs.as_ref())?;
                        ch.push_binding(ident.clone(), VarOp::Set)?;
                        // let ty = ast.symbols.var_type(ident.clone());
                        // if let Some(VarType::Var) = ty {
                        //     Self::compile_expr(ch, rhs.as_ref())?;
                        //     ch.push_binding(ident.clone(), VarOp::Set)?;
                        // } else {
                        //     bail!("Cannot assign to a binding that was not declared as 'var'. ident: {}, var_type: {ty:?}", ident.name());
                        // }
                    }
                    FormExpr::Call { callee, params } => {
                        match callee.as_ref() {
                            Expr::Atom(ZValue::Ident(name)) => {}

                            // Expr::Atom(ZValue::Func(cu)) => {}
                            // Expr::Form(FormExpr::Call { .. }) => {
                            // Self::compile_expr(ch, c)?;
                            // }
                            _ => {}
                        }
                        // if let = callee {
                        //
                        // } else {
                        //     Self::compile_expr(ch, callee);
                        // }
                        let nargs = if let Some(ps) = params {
                            for param in ps.iter() {
                                Self::compile_expr(ch, param)?;
                            }
                            ps.len()
                        } else {
                            // NOTE: Implicit nil if no args.
                            1
                        } as u64;
                        let op_param = OpParam::squash(nargs);
                        ch.push_opcode(Opcode::with_param(Op::Call, op_param));
                    }
                    FormExpr::Return { expr } => {
                        if let Some(e) = expr {
                            Self::compile_expr(ch, e)?;
                        } else {
                            let _ = ch.push_opcode(Opcode::new(Op::Nil));
                        }
                        let _ = ch.push_opcode(Opcode::new(Op::Return));
                    }
                    _ => bail!("Unspecified error in compile_expr matching CallExpr::Std(StdForm)"),
                }
            }

            Expr::Binding { ty, name, init } => {
                Self::compile_expr(ch, init.as_ref())?;
                let name = name.expect_ident();
                ch.declare_binding(name.clone())?;
            }
            Expr::List(li) => Self::compile_expr_list(ch, li)?,
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
                ZValue::Func(fu) => todo!(),
            },

            Expr::Nil => {
                ch.push_opcode(Opcode::new(Op::Nil));
            }
            Expr::Function(func_decl) => todo!(),
        }
        Ok(())
    }
}
