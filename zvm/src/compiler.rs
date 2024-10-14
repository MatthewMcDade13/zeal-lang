//Bytecode compiler for ZealVM bytecode

use anyhow::bail;
use zeal_ast::{
    expr::{AstList, EscapeExpr, Expr, ExprStmt, WhenForm},
    Ast,
};

use crate::{
    chunk::{Chunk, ChunkBuilder, FuncChunk},
    opcode::{BinaryOpType, Op, OpParam, Opcode, VarOp},
};

pub struct Archon;

impl Archon {
    pub fn compile_entrypoint(ast: &Ast) -> anyhow::Result<FuncChunk> {
        println!("{ast}");
        let mut ch = ChunkBuilder::default();
        Self::compile_with(ast, &mut ch)?;
        let ch = ch.build_fn("main", 0);

        println!("{ch}");
        Ok(ch)
    }

    pub fn compile_with(ast: &Ast, ch: &mut ChunkBuilder) -> anyhow::Result<()> {
        if let AstList::List(l) = &ast.tree {
            for s in l.iter() {
                Self::compile_expr_stmt(ch, s)?;
            }
        } else {
            bail!("Cannot compile empty AST!");
        }

        Ok(())
    }

    fn compile_logical_op(
        ch: &mut ChunkBuilder,
        operator: (BinaryOpType, &Expr, &Expr),
    ) -> anyhow::Result<()> {
        let (ty, left, right) = operator;
        let op = match ty {
            BinaryOpType::And => Op::JumpFalse,
            BinaryOpType::Or => Op::JumpTrue,
            _ => bail!(
                "compile_logical_op: Tried to compile logical operator but was given type: {ty:?}"
            ),
        };

        Self::compile_expr(ch, left)?;
        let jump = ch.push_jump(op);
        ch.push_popn(1);
        Self::compile_expr(ch, right)?;
        ch.patch_jump(jump);
        Ok(())
    }

    fn compile_expr_stmt(ch: &mut ChunkBuilder, el: &ExprStmt) -> anyhow::Result<()> {
        match el {
            ExprStmt::Block(bl) => {
                ch.start_scope();
                for e in bl.iter()? {
                    Self::compile_expr_stmt(ch, e)?;
                }
                let pops = ch.end_scope();
                if pops > 0 {
                    ch.push_popn(pops as u8);
                }
            }
            ExprStmt::Loop(ast_list) => {
                let mut breaks = Vec::new();
                ch.start_scope();
                let top = ch.len();
                for ex in ast_list.iter()? {
                    if matches!(ex, ExprStmt::Escape(EscapeExpr::Break { .. })) {
                        let jump = ch.push_jump(Op::Jump);
                        breaks.push(jump);
                    } else {
                        Self::compile_expr_stmt(ch, ex)?;
                    }
                }
                let jump_op = Opcode::WithParam {
                    op: Op::Jump,
                    param: OpParam::byte16(top as u16),
                };
                ch.push_opcode(jump_op);
                let pops = ch.end_scope();
                for b in breaks {
                    ch.patch_jump(b);
                }
                if pops > 0 {
                    ch.push_popn(pops as u8);
                }
            }
            ExprStmt::While { cond, body } => {
                let mut breaks = Vec::new();
                ch.start_scope();
                let top = ch.len();
                for ex in body.iter()? {
                    if matches!(ex, ExprStmt::Escape(EscapeExpr::Break { .. })) {
                        let jump = ch.push_jump(Op::Jump);
                        breaks.push(jump);
                    } else {
                        Self::compile_expr_stmt(ch, ex)?;
                    }
                }
                Self::compile_expr(ch, cond)?;
                let jump_op = Opcode::WithParam {
                    op: Op::JumpTrue,
                    param: OpParam::byte16(top as u16),
                };
                ch.push_opcode(jump_op);
                let pops = ch.end_scope();
                for b in breaks {
                    ch.patch_jump(b);
                }
                if pops > 0 {
                    ch.push_popn(pops as u8);
                }
            }
            ExprStmt::When(ast_list) => {
                let mut end_patches = Vec::new();
                for ce in ast_list.iter()? {
                    match ce {
                        WhenForm::Branch(cond, block) => {
                            Self::compile_expr(ch, cond)?;
                            let cond_jump = ch.push_jump(Op::JumpFalse);
                            ch.push_popn(1);
                            Self::compile_expr_stmt(ch, block)?;

                            {
                                let end_jump = ch.push_jump(Op::Jump);
                                end_patches.push(end_jump);
                            }

                            ch.patch_jump(cond_jump);
                            ch.push_popn(1);
                        }
                        WhenForm::Else(block) => {
                            println!("Compiling Else Block: {block:?}, {}", block.type_str());
                            Self::compile_expr_stmt(ch, block)?;
                            break;
                        }
                        WhenForm::End => break,
                    }
                }
                for patch in end_patches.iter() {
                    ch.patch_jump(*patch);
                }

                // Self::compile_expr( ch, cond)?;
                //
                // let then_patch = ch.push_jump(Op::JumpFalse);
                // ch.push_popn(1);
                // Self::compile_expr_list( ch, then_body)?;
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
                //         Self::compile_expr_list( ch, &ExprList::Block(bl.clone()))?;
                //     }
                //     // elseif
                //     Expr::Form(FormExpr::Cond { .. }) => Self::compile_expr( ch, eb.as_ref())?,
                //     _ => bail!("CompileError => Expected Expr::List(ExprList::Block()) | Expr::Call(CallExpr::If {{..}}) in Archon::compile_expr for else body!!! Probably missing ending else/end/elseif!!!"),
                // }
                // }
                // ch.patch_jump(else_jump);
            }
            ExprStmt::DefFunc(func_decl) => {}
            ExprStmt::Binding(bind_stmt) => {}
            ExprStmt::Escape(EscapeExpr::Return(e)) => {
                todo!()
            }
            ExprStmt::Escape(es) => {
                bail!("Illegal use of Escape Statement: {es:?}. Breaks and Continues must be inside a loop!!!");
            }
            ExprStmt::Atom(expr) => Self::compile_expr(ch, expr)?,
        }
        Ok(())
    }

    fn compile_expr(ch: &mut ChunkBuilder, expr: &Expr) -> anyhow::Result<()> {
        match expr {
            Expr::Assign { lhs, rhs } => {
                if let Expr::Rune(r) = lhs.as_ref() {
                    Self::compile_expr(ch, rhs.as_ref())?;
                    ch.push_binding(r.as_ref(), VarOp::Set)?;
                } else {
                    // NOTE: For now, only identifiers can be reassigned. Eventually
                    // we will allow assignment expressions like: foo.bar() = baz;
                    bail!("Only identifiers can be assigned!")
                }
            }
            Expr::Rune(rc) => todo!(),
            Expr::Bool(_) => todo!(),
            Expr::Byte(_) => todo!(),
            Expr::SByte(_) => todo!(),
            Expr::Int(_) => todo!(),
            Expr::Uint(_) => todo!(),
            Expr::Float(_) => todo!(),
            Expr::String(rc) => todo!(),
            Expr::List(ast_list) => todo!(),
            Expr::Pair(_) => todo!(),
            Expr::Triple(_) => todo!(),
            Expr::Call { head, args } => todo!(),
            Expr::Unit => todo!(),
        }
        Ok(())
    }
}
