//Bytecode compiler for ZealVM bytecode

use anyhow::{bail, ensure};
use zeal_ast::{
    expr::{AstList, BindStmt, EscapeExpr, Expr, ExprStmt, FuncDecl, OperatorType, WhenForm},
    Ast,
};

use crate::{
    chunk::{Chunk, ChunkBuilder, FuncChunk},
    opcode::{Op, OpParam, Opcode, VarOp},
    val::Val,
};

pub struct Archon;

impl Archon {
    pub fn compile_entrypoint(ast: &Ast) -> anyhow::Result<FuncChunk> {
        println!("{ast}");
        let mut ch = ChunkBuilder::default();
        Self::compile_with(ast, &mut ch)?;
        let ch = ch.build_func("__main__", 0);

        // println!("{ch}");
        Ok(ch)
    }

    pub fn compile_func(decl: &FuncDecl) -> anyhow::Result<FuncChunk> {
        let mut ch = ChunkBuilder::default();
        ch.push_constant(Val::rune(decl.name.as_ref()));
        Self::compile_expr_stmt(&mut ch, decl.body.as_ref())?;

        let ch = ch.build_func(&decl.name, decl.arity() as u8);
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
            // TODO: Right now we just have breaks and no continues....
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
            }
            ExprStmt::DefFunc(FuncDecl { name, params, body }) => {}
            ExprStmt::Binding(BindStmt { name, rhs, .. }) => {
                Self::compile_expr(ch, rhs)?;
                ch.declare_binding(name.as_ref())?;
            }
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
            Expr::Rune(rc) => ch.push_binding(rc.as_ref(), VarOp::Get)?,
            Expr::Bool(true) => {
                let _ = ch.push_opcode(Opcode::new(Op::True));
            }
            Expr::Bool(false) => {
                let _ = ch.push_opcode(Opcode::new(Op::False));
            }
            Expr::Byte(b) => {
                let _ = ch.push_constant(Val::Byte(*b));
            }
            Expr::SByte(b) => {
                let _ = ch.push_constant(Val::SByte(*b));
            }
            Expr::Int(i) => {
                let _ = ch.push_constant(Val::Num(*i));
            }
            Expr::Uint(ui) => {
                let _ = ch.push_constant(Val::UNum(*ui));
            }
            Expr::Float(fl) => {
                let _ = ch.push_constant(Val::Float(*fl));
            }
            Expr::String(rc) => {
                let _ = ch.push_string(rc.as_ref());
            }
            Expr::List(ast_list) => {
                if let AstList::List(l) = ast_list {
                    for ex in l.iter() {
                        Self::compile_expr(ch, ex)?;
                    }
                }
            }
            Expr::Pair(p) => {
                Self::compile_expr(ch, &p[0])?;
                Self::compile_expr(ch, &p[1])?;
            }
            Expr::Triple(tr) => {
                Self::compile_expr(ch, &tr[0])?;
                Self::compile_expr(ch, &tr[1])?;
                Self::compile_expr(ch, &tr[2])?;
            }
            Expr::Call { head, args } => {
                // ensure!(
                //     matches!(args.as_ref(), Expr::List(_)),
                //     "args of call expression must be a list!"
                // );
                Self::compile_expr(ch, args.as_ref())?;

                ensure!(
                    matches!(head.as_ref(), Expr::Rune(_)),
                    "Head of call expression must be a rune!"
                );
                Self::compile_expr(ch, head.as_ref())?;
            }
            Expr::Unit => {
                let _ = ch.push_constant(Val::Unit);
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
                        return Self::compile_logical_op(ch, (*ty, &exs[0], &exs[1]));
                    }

                    zeal_ast::expr::OperatorType::Unknown => todo!(),
                };

                for e in args.assert_list()? {
                    Self::compile_expr(ch, e)?;
                }
                ch.push_opcode(Opcode::new(op));
            }
        }
        Ok(())
    }
}
