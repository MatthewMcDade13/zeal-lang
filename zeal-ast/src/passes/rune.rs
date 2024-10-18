use zeal_core::rune::{RuneTable, RuneTableBuilder};

use crate::{
    ast_node,
    expr::{AstList, BindStmt, Binding, Expr, ExprStmt, FuncDecl, WhenForm},
    Ast, AstWalker,
};

pub struct RuneTablePass {
    table: RuneTableBuilder,
}

impl RuneTablePass {
    pub fn dopass(ast: &Ast) -> anyhow::Result<RuneTable> {
        let mut s = Self {
            table: RuneTableBuilder::default(),
        };

        if let AstList::List(al) = &ast.tree {
            for stmt in al.iter() {
                s.visit(stmt)?;
            }
        }

        let t = s.table.build();
        Ok(t)
    }
}

// ast_node!(ExprStmt, ());

impl AstWalker<ExprStmt> for RuneTablePass {
    fn visit(&mut self, node: &ExprStmt) -> anyhow::Result<()> {
        match node {
            ExprStmt::Loop(ast_list) | ExprStmt::Block(ast_list) => {
                if let AstList::List(al) = ast_list {
                    for ex in al.iter() {
                        ex.walk(self)?;
                    }
                }
            }
            ExprStmt::While { body, .. } => {
                if let AstList::List(al) = &body {
                    for ex in al.iter() {
                        ex.walk(self)?;
                    }
                }
            }
            ExprStmt::When(ast_list) => {
                if let AstList::List(branches) = ast_list {
                    for wh in branches.iter() {
                        match wh {
                            WhenForm::Branch(_, expr_stmt) => expr_stmt.walk(self)?,
                            WhenForm::Else(expr_stmt) => expr_stmt.walk(self)?,
                            WhenForm::End => break,
                        }
                    }
                }
            }
            ExprStmt::DefFunc(FuncDecl { name, params, body }) => {
                self.table.add_rune(name);
                if let AstList::List(ps) = params {
                    for Binding { name, .. } in ps.iter() {
                        self.table.add_rune(name);
                    }
                }

                body.walk(self)?;
            }
            ExprStmt::Binding(BindStmt { name, .. }) => {
                self.table.add_rune(name);
            }
            ExprStmt::Escape(_) => {}
            ExprStmt::Atom(expr) => expr.walk(self)?,
        }
        Ok(())
    }
}

impl AstWalker<Expr> for RuneTablePass {
    fn visit(&mut self, node: &Expr) -> anyhow::Result<()> {
        match node {
            Expr::Rune(rc) => self.table.add_rune(rc.as_ref()),
            Expr::String(rc) => self.table.add_rune(rc.as_ref()),
            Expr::List(AstList::List(al)) => {
                for ex in al.iter() {
                    ex.walk(self)?;
                }
            }
            _ => {}
        }
        Ok(())
    }
}
