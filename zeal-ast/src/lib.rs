pub mod err;
pub mod expr;
pub mod lex;
pub mod parse;
pub mod typecheck;

use anyhow::bail;
use expr::{AstList, Expr, ExprStmt};
use lex::{Tok, TokBuffer};
use parse::Parser;

use std::{fmt::Display, rc::Rc};

pub trait AstWalker<Node, R>
where
    Self: Sized,
{
    fn visit(&mut self, node: &Node) -> anyhow::Result<R>;
}

pub trait AstNode<W: AstWalker<Self, R>, R>
where
    Self: Sized,
{
    fn walk(&self, walker: &mut W) -> anyhow::Result<R> {
        walker.visit(self)
    }
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub tree: AstList<ExprStmt>,
}

impl Ast {
    pub fn from_toks(toks: &[Tok]) -> anyhow::Result<Self> {
        let ast = Parser::parse_ast(toks)?;
        // let symbols = Self::build_symbol_table(&tree);
        Ok(ast)
        // Ok(Self(ast))
    }

    pub fn from_file(path: &str) -> anyhow::Result<Self> {
        let buf = TokBuffer::read_file(path)?;
        let s = Self::from_toks(buf.slice())?;
        Ok(s)
    }
}

impl std::str::FromStr for Ast {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let buf = TokBuffer::read_string(s)?;
        let s = Self::from_toks(buf.slice())?;
        Ok(s)
    }
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = String::from("----- AST ----- \n");
        if let AstList::List(l) = &self.tree {
            for e in l.iter() {
                if let Ok(est) = AstStringify::expr_stmt_tostring(e) {
                    res.push_str(&format!("{est}\n"));
                } else {
                    return std::fmt::Result::Err(std::fmt::Error);
                }
            }
        }
        res.push_str("----- END AST -----\n");
        write!(f, "{}", res)
    }
}

macro_rules! ast_node {
    ($node: ty, $output: ty) => {
        impl $node {
            pub fn walk<T>(&self, walker: &mut T) -> anyhow::Result<$output>
            where
                T: AstWalker<Self, $output>,
            {
                walker.visit(self)
            }
        }
    };
}

ast_node!(ExprStmt, String);
ast_node!(Expr, String);

#[derive(Debug, Clone, Copy)]
pub struct AstStringify;

impl AstStringify {
    #[inline]
    pub fn expr_tostring(expr: &Expr) -> anyhow::Result<String> {
        expr.walk(&mut Self)
    }

    #[inline]
    pub fn expr_stmt_tostring(est: &ExprStmt) -> anyhow::Result<String> {
        est.walk(&mut Self)
    }
}

impl AstWalker<ExprStmt, String> for AstStringify {
    fn visit(&mut self, node: &ExprStmt) -> anyhow::Result<String> {
        let s = match node {
            ExprStmt::Block(ast_list) => {
                let mut s = String::from("(block \n");
                if let AstList::List(al) = ast_list {
                    for est in al.iter() {
                        let mut es = String::from("\t");
                        es.push_str(&est.walk(self)?);
                        es.push('\n');
                        s.push_str(&es);
                    }
                }

                s.push_str("\tend)");
                s
            }
            ExprStmt::Loop(ast_list) => {
                let mut s = String::from("(loop \n");
                if let AstList::List(al) = ast_list {
                    for est in al.iter() {
                        s.push_str(&format!("\t{}\n", &est.walk(self)?));
                    }
                }
                s.push_str("end)");
                s
            }
            ExprStmt::While { cond, body } => {
                let mut s = format!("(while {}\n", cond.walk(self)?);
                if let AstList::List(b) = body {
                    for est in b.iter() {
                        s.push_str(&format!("\t{}\n", est.walk(self)?));
                    }
                }
                s.push_str("end)");
                s
            }
            ExprStmt::When(conds) => {
                let mut s = String::from("(when \n");
                if let AstList::List(cs) = conds {
                    for cond in cs.iter() {
                        match cond {
                            expr::WhenForm::Branch(expr, expr_stmt) => {
                                let bs = format!(
                                    "\t{} => {}\n",
                                    expr.walk(self)?,
                                    expr_stmt.walk(self)?
                                );
                                s.push_str(&bs);
                            }
                            expr::WhenForm::Else(expr_stmt) => {
                                let bs = format!("\telse => {}\n", expr_stmt.walk(self)?);
                                s.push_str(&bs);
                            }
                            expr::WhenForm::End => {
                                s.push_str("\tend)");
                            }
                        }
                    }

                    s
                } else {
                    bail!("Illegal When Statment with no conditions!!")
                }
            }
            ExprStmt::DefFunc(func_decl) => {
                let s = format!(
                    "{}/{}\n\t{}",
                    func_decl.name.as_ref(),
                    if let AstList::List(params) = &func_decl.params {
                        let mut ps = String::new();
                        for b in params.iter() {
                            ps.push_str(&format!(
                                "{} {}",
                                b.name.as_ref(),
                                b.typename.as_ref().unwrap_or(&Rc::from(""))
                            ));
                        }
                        ps
                    } else {
                        String::from("()")
                    },
                    func_decl.body.walk(self)?
                );
                s
            }
            ExprStmt::Binding(bind_stmt) => bind_stmt.to_string(),
            ExprStmt::Escape(escape_expr) => escape_expr.to_string(),
            ExprStmt::Atom(expr) => expr.walk(self)?,
        };
        Ok(s)
    }
}

impl AstWalker<Expr, String> for AstStringify {
    fn visit(&mut self, node: &Expr) -> anyhow::Result<String> {
        let s = match node {
            Expr::Rune(rc) => format!(":{}", rc.as_ref()),
            Expr::Bool(b) => b.to_string(),
            Expr::Byte(b) => b.to_string(),
            Expr::SByte(b) => b.to_string(),
            Expr::Int(i) => i.to_string(),
            Expr::Uint(ui) => ui.to_string(),
            Expr::Float(f) => f.to_string(),
            Expr::String(s) => format!("\"{}\"", s.as_ref()),
            Expr::List(l) => {
                let mut s = String::from("[");
                if let AstList::List(es) = l {
                    for e in es.iter() {
                        s.push_str(&e.walk(self)?);
                        s.push(' ');
                    }
                } else {
                    s.push(']');
                };
                s
            }
            Expr::Pair(p) => format!("[|{}, {}|]", p[0].walk(self)?, p[1].walk(self)?),
            Expr::Triple(t) => format!(
                "[|{}, {}, {}|]",
                t[0].walk(self)?,
                t[1].walk(self)?,
                t[2].walk(self)?
            ),
            Expr::Assign { lhs, rhs } => format!("(= {} {})", lhs.walk(self)?, rhs.walk(self)?),
            Expr::Call { head, args } => format!("({} {})", head.walk(self)?, args.walk(self)?),
            Expr::Unit => String::from("()"),
        };
        Ok(s)
    }
}
