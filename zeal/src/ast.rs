use std::rc::Rc;

use anyhow::bail;
use thiserror::Error;

use crate::{
    core_types::{str::ZSymbol, ZValue},
    lex::{LexTok, Tok},
    parse::Parser,
};

#[derive(Debug, Clone)]
pub struct Ast(Box<[Expr]>);

impl Ast {
    pub fn from_toks(toks: &[Tok]) -> anyhow::Result<Self> {
        let ast = Parser::parse_ast(toks)?;
        let ast = ast.into_boxed_slice();
        Ok(Self(ast))
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Call(Rc<[Expr]>),
    Block(Rc<[Expr]>),

    // DoBlock(),
    Group(Rc<Expr>),
    Atom(ZValue),
}

impl Expr {
    pub fn assignment(name: &Expr, value: &Expr) -> Self {
        let ast = [
            Self::Atom(ZValue::Sym(ZSymbol::from("="))),
            name.clone(),
            value.clone(),
            // Self::Atom(name.clone()),
            // Self::Atom(value.clone()),
        ];
        let ast = Rc::from(ast);
        Self::Call(ast)
    }

    pub fn binary_op(left: &Expr, op: &ZSymbol, right: &Expr) -> Self {
        let ast = [
            Self::Atom(ZValue::Sym(op.clone())),
            left.clone(),
            right.clone(),
            // Self::Atom(left.clone()),
            // Self::Atom(right.clone()),
        ];
        let ast = Rc::from(ast);
        Self::Call(ast)
    }

    pub fn unary_op(op: &ZSymbol, right: &Expr) -> Self {
        let ast = [
            Self::Atom(ZValue::Sym(op.clone())),
            right.clone(),
            // Self::Atom(left.clone()),
            // Self::Atom(right.clone()),
        ];
        let ast = Rc::from(ast);
        Self::Call(ast)
    }

    pub const fn as_zval(&self) -> Option<&ZValue> {
        match self {
            Self::Atom(v) => Some(v),
            _ => None,
        }
    }

    pub const fn as_zval_sym(&self) -> Option<&ZValue> {
        if let Some(v) = self.as_zval() {
            if v.is_sym() {
                Some(v)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub const fn inner_symbol(&self) -> Option<&ZSymbol> {
        match self {
            Self::Atom(v) => match v {
                ZValue::Sym(s) => Some(s),
                _ => None,
            },
            _ => None,
        }
    }

    pub const fn is_symbol(&self) -> bool {
        if let Some(_) = self.inner_symbol() {
            true
        } else {
            false
        }
    }
}

// #[derive(Debug, Clone)]
// pub enum Expr {
//     Binary {
//         left: Box<Expr>,
//         operator: Tok,
//         right: Box<Expr>,
//     },
//     Grouping(Box<Expr>),
//     Literal(ZValue),
//     Unary {
//         operator: Tok,
//         right: Box<Expr>,
//     },
//     Assignment {
//         name: Tok,
//         value: Box<Expr>,
//     },
//     Name(Tok),
// }
//
// impl Expr {
//     pub fn walk<T, R>(&self, visitor: &mut T) -> anyhow::Result<R>
//     where
//         T: AstWalker<Self, R>,
//     {
//         visitor.visit(self)
//     }
// }
//
// #[derive(Debug, Clone)]
// pub enum Stmt {
//     Block(Vec<Stmt>),
//     Expression(Expr),
//     Print(Expr),
//     Let {
//         name: LexTok,
//         initializer: Option<Expr>,
//     },
// }
// impl Stmt {
//     pub fn walk<T, R>(&self, visitor: &mut T) -> anyhow::Result<R>
//     where
//         T: AstWalker<Self, R>,
//     {
//         visitor.visit(self)
//     }
// }

// pub struct AstStringify;
//
// pub trait AstWalker<T, R> {
//     fn visit(&mut self, node: &T) -> anyhow::Result<R>;
// }
//
#[derive(Error, Clone, Debug)]
pub enum AstWalkError {
    #[error("Runtime Error :: {token:?} => {message}")]
    RuntimeError { token: LexTok, message: String },
    #[error("Type Error :: {value:?} => {message}")]
    TypeError { value: ZValue, message: String },
    #[error("Parse Error :: {token:?} - {message}")]
    ParseError { token: LexTok, message: String },
}
// impl AstStringify {
//     pub fn stringify(&mut self, e: &Expr) -> anyhow::Result<String> {
//         e.walk(self)
//     }
//
//     pub fn lispify(&mut self, name: &str, exprs: &[&Expr]) -> anyhow::Result<String> {
//         let mut result = String::new();
//         result.push_str(&format!("({name}"));
//         for expr in exprs {
//             result.push_str(&format!(" {}", expr.walk(self)?));
//         }
//         result.push_str(")");
//         Ok(result)
//     }
// }
//
// impl AstWalker<Expr, String> for AstStringify {
//     fn visit(&mut self, expr: &Expr) -> anyhow::Result<String> {
//         match expr {
//             Expr::Binary {
//                 left,
//                 operator,
//                 right,
//             } => self.lispify(&operator.lexeme, &[left.as_ref(), right.as_ref()]),
//             Expr::Grouping(exp) => self.lispify("group", &[&exp.as_ref()]),
//             Expr::Literal(lit) => match lit {
//                 ZValue::Nil => Ok("nil".into()),
//
//                 _ => Ok(lit.to_string()),
//             },
//             Expr::Unary { operator, right } => self.lispify(&operator.lexeme, &[&right.as_ref()]),
//             Expr::Name(name) => Ok(name.lexeme.clone()),
//             Expr::Assignment { name, value } => self.lispify(&name.lexeme, &[&value.as_ref()]),
//         }
//     }
// }
