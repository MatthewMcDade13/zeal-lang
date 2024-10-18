//Bytecode compiler for ZealVM bytecode

use std::rc::Rc;

use anyhow::{bail, ensure};
use zeal_ast::{
    expr::{AstList, BindStmt, EscapeExpr, Expr, ExprStmt, FuncDecl, OperatorType, WhenForm},
    Ast,
};

use crate::{
    chunk::{Chunk, ChunkBuilder, FuncChunk},
    env::CompileEnv,
    opcode::{Op, OpParam, Opcode, VarOp},
    val::Val,
};

pub struct Archon;

impl Archon {
    pub fn compile_entrypoint(ast: &Ast) -> anyhow::Result<FuncChunk> {
        let mut env = CompileEnv::root();
        Self::compile_with(ast, &mut env)?;
        let ch = env.state.build_func("__main__", 0);

        // println!("{ch}");
        Ok(ch)
    }

    pub fn compile_func(decl: &FuncDecl, parent: &mut ChunkBuilder) -> anyhow::Result<()> {
        let mut cb = ChunkBuilder::default();
        Self::compile_expr_stmt(&mut cb, decl.body.as_ref())?;

        cb.build_func_in(&mut parent.0, &decl.name, decl.arity() as u8);
        Ok(())

        // ch.push_constant(Val::Func(Rc::new(fc)));
    }

    pub fn compile_with(ast: &Ast, env: &mut CompileEnv) -> anyhow::Result<()> {
        if let AstList::List(l) = &ast.tree {
            for s in l.iter() {
                Self::compile_expr_stmt(env, s)?;
            }
        } else {
            bail!("Cannot compile empty AST!");
        }

        Ok(())
    }

    fn compile_logical_op(
        cb: &mut ChunkBuilder,
        operator: (OperatorType, &Expr, &Expr),
    ) -> anyhow::Result<()> {
        let (ty, left, right) = operator;
        let op = match ty {
            OperatorType::And => Op::JumpFalse,
            OperatorType::Or => Op::JumpTrue,
            _ => bail!(
                "compile_logical_op: Tried to compile logical operator but was given type: {ty:?}"
            ),
        };

        Self::compile_expr(cb, left)?;
        let jump = cb.push_jump(op);
        cb.push_popn(1);
        Self::compile_expr(cb, right)?;
        cb.patch_jump(jump);
        Ok(())
    }

    fn compile_expr_stmt(cb: &mut ChunkBuilder, el: &ExprStmt) -> anyhow::Result<()> {
        match el {
            ExprStmt::Block(bl) => {
                cb.start_scope();
                for e in bl.iter()? {
                    Self::compile_expr_stmt(cb, e)?;
                }
                let pops = cb.end_scope();
                if pops > 0 {
                    cb.push_popn(pops as u8);
                }
            }
            // TODO: Right now we just have breaks and no continues....
            ExprStmt::Loop(ast_list) => {
                let mut breaks = Vec::new();
                cb.start_scope();
                let top = cb.len();
                for ex in ast_list.iter()? {
                    if matches!(ex, ExprStmt::Escape(EscapeExpr::Break { .. })) {
                        let jump = cb.push_jump(Op::Jump);
                        breaks.push(jump);
                    } else {
                        Self::compile_expr_stmt(cb, ex)?;
                    }
                }
                let jump_op = Opcode::WithParam {
                    op: Op::Jump,
                    param: OpParam::byte16(top as u16),
                };
                cb.push_opcode(jump_op);
                let pops = cb.end_scope();
                for b in breaks {
                    cb.patch_jump(b);
                }
                if pops > 0 {
                    cb.push_popn(pops as u8);
                }
            }
            ExprStmt::While { cond, body } => {
                let mut breaks = Vec::new();
                cb.start_scope();
                let top = cb.len();
                for ex in body.iter()? {
                    if matches!(ex, ExprStmt::Escape(EscapeExpr::Break { .. })) {
                        let jump = cb.push_jump(Op::Jump);
                        breaks.push(jump);
                    } else {
                        Self::compile_expr_stmt(cb, ex)?;
                    }
                }
                Self::compile_expr(cb, cond)?;
                let jump_op = Opcode::WithParam {
                    op: Op::JumpTrue,
                    param: OpParam::byte16(top as u16),
                };
                cb.push_opcode(jump_op);
                let pops = cb.end_scope();
                for b in breaks {
                    cb.patch_jump(b);
                }
                if pops > 0 {
                    cb.push_popn(pops as u8);
                }
            }
            ExprStmt::When(ast_list) => {
                let mut end_patches = Vec::new();
                for ce in ast_list.iter()? {
                    match ce {
                        WhenForm::Branch(cond, block) => {
                            Self::compile_expr(cb, cond)?;
                            let cond_jump = cb.push_jump(Op::JumpFalse);
                            cb.push_popn(1);
                            Self::compile_expr_stmt(cb, block)?;

                            {
                                let end_jump = cb.push_jump(Op::Jump);
                                end_patches.push(end_jump);
                            }

                            cb.patch_jump(cond_jump);
                            cb.push_popn(1);
                        }
                        WhenForm::Else(block) => {
                            println!("Compiling Else Block: {block:?}, {}", block.type_str());
                            Self::compile_expr_stmt(cb, block)?;
                            break;
                        }
                        WhenForm::End => break,
                    }
                }
                for patch in end_patches.iter() {
                    cb.patch_jump(*patch);
                }
            }
            ExprStmt::DefFunc(decl) => {
                let fc = Self::compile_func(decl, cb);
            }
            ExprStmt::Binding(BindStmt { name, rhs, .. }) => {
                Self::compile_expr(cb, rhs)?;
                cb.declare_binding(name.as_ref())?;
            }
            ExprStmt::Escape(EscapeExpr::Return(e)) => {
                todo!()
            }
            ExprStmt::Escape(es) => {
                bail!("Illegal use of Escape Statement: {es:?}. Breaks and Continues must be inside a loop!!!");
            }
            ExprStmt::Atom(expr) => Self::compile_expr(cb, expr)?,
        }
        Ok(())
    }

    fn compile_expr(cb: &mut ChunkBuilder, expr: &Expr) -> anyhow::Result<()> {
        match expr {
            Expr::Assign { lhs, rhs } => {
                if let Expr::Rune(r) = lhs.as_ref() {
                    Self::compile_expr(cb, rhs.as_ref())?;
                    cb.push_binding(r.as_ref(), VarOp::Set)?;
                } else {
                    // NOTE: For now, only identifiers can be reassigned. Eventually
                    // we will allow assignment expressions like: foo.bar() = baz;
                    bail!("Only identifiers can be assigned!")
                }
            }
            Expr::Rune(rc) => cb.push_binding(rc.as_ref(), VarOp::Get)?,
            Expr::Bool(true) => {
                let _ = cb.push_opcode(Opcode::new(Op::True));
            }
            Expr::Bool(false) => {
                let _ = cb.push_opcode(Opcode::new(Op::False));
            }
            Expr::Byte(b) => {
                let _ = cb.push_constant(Val::Byte(*b));
            }
            Expr::SByte(b) => {
                let _ = cb.push_constant(Val::SByte(*b));
            }
            Expr::Int(i) => {
                let _ = cb.push_constant(Val::Num(*i));
            }
            Expr::Uint(ui) => {
                let _ = cb.push_constant(Val::UNum(*ui));
            }
            Expr::Float(fl) => {
                let _ = cb.push_constant(Val::Float(*fl));
            }
            Expr::String(rc) => {
                let _ = cb.push_string(rc.as_ref());
            }
            Expr::List(ast_list) => {
                if let AstList::List(l) = ast_list {
                    for ex in l.iter() {
                        Self::compile_expr(cb, ex)?;
                    }
                }
            }
            Expr::Pair(p) => {
                Self::compile_expr(cb, &p[0])?;
                Self::compile_expr(cb, &p[1])?;
            }
            Expr::Triple(tr) => {
                Self::compile_expr(cb, &tr[0])?;
                Self::compile_expr(cb, &tr[1])?;
                Self::compile_expr(cb, &tr[2])?;
            }
            Expr::Call { head, args } => {
                ensure!(
                    matches!(head.as_ref(), Expr::Rune(_)),
                    "Head of call expression must be a rune!"
                );
                Self::compile_expr(cb, head.as_ref())?;

                // ensure!(
                //     matches!(args.as_ref(), Expr::List(_)),
                //     "args of call expression must be a list!"
                // );
                Self::compile_expr(cb, args.as_ref())?;
                if let Some(ls) = args.try_list() {
                    cb.push_opcode(Opcode::with_param(
                        Op::Call,
                        OpParam::squash(ls.len() as u64),
                    ));
                } else {
                    // NOTE: This is a scalar value
                    cb.push_opcode(Opcode::with_param(Op::Call, OpParam::Byte(1)));
                }
            }
            Expr::Unit => {
                let _ = cb.push_constant(Val::Unit);
            }
            Expr::Operator { ty, args } => {
                let op = match ty {
                    OperatorType::Add => Op::Add,
                    OperatorType::Sub => Op::Sub,
                    OperatorType::Mul => Op::Mul,
                    OperatorType::Div => Op::Div,
                    OperatorType::Concat => Op::Concat,
                    OperatorType::Negate => Op::Neg,
                    OperatorType::Not => Op::Not,
                    OperatorType::Modulo => todo!(),
                    OperatorType::Gt => Op::Gt,
                    OperatorType::Lt => Op::Lt,
                    OperatorType::Ge => Op::Ge,
                    OperatorType::Le => Op::Le,
                    OperatorType::Eq => Op::Eq,
                    OperatorType::NotEq => Op::NotEq,
                    OperatorType::Or | OperatorType::And => {
                        let exs = args.assert_list()?;
                        ensure!(
                            exs.len() >= 2,
                            "Cannot compile logical operator with less than 2 args!!"
                        );
                        return Self::compile_logical_op(cb, (*ty, &exs[0], &exs[1]));
                    }

                    zeal_ast::expr::OperatorType::Unknown => todo!(),
                };

                for e in args.assert_list()? {
                    Self::compile_expr(cb, e)?;
                }
                cb.push_opcode(Opcode::new(op));
            }
        }
        Ok(())
    }
}
