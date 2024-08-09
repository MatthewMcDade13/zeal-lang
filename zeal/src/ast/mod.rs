pub mod lex;
pub mod parse;
pub mod reduce;

use crate::ast::lex::{LexTok, Tok, TokBuffer};
use crate::ast::parse::Parser;
use crate::compiler::opcode::Op;
use crate::core_types::keywords;
use std::{fmt::Display, ops::Index, rc::Rc};

use thiserror::Error;

use crate::core_types::{str::ZSymbol, val::ZValue};

#[derive(Debug, Clone)]
pub struct Ast(Box<[Expr]>);

impl Ast {
    pub fn empty() -> Self {
        let vec: Vec<Expr> = vec![];
        Self(vec.into_boxed_slice())
    }

    pub fn from_toks(toks: &[Tok]) -> anyhow::Result<Self> {
        let ast = Parser::parse_ast(toks)?;
        let ast = ast.into_boxed_slice();
        Ok(Self(ast))
    }

    pub fn from_str(str: &str) -> anyhow::Result<Self> {
        let buf = TokBuffer::read_string(str)?;
        let s = Self::from_toks(buf.slice())?;
        Ok(s)
    }

    pub fn from_file(path: &str) -> anyhow::Result<Self> {
        let buf = TokBuffer::read_file(path)?;
        let s = Self::from_toks(buf.slice())?;
        Ok(s)
    }

    pub fn concat(&self, other: &Self) -> Self {
        let s = self.0.as_ref();
        let other = other.0.as_ref();
        let res = ([s, other]).concat().into_boxed_slice();
        Self(res)
    }

    pub fn append(&mut self, other: Self) {
        let s = self.0.as_ref();
        let other = other.0.as_ref();
        self.0 = ([s, other]).concat().into_boxed_slice();
    }

    pub fn slice(&self) -> &[Expr] {
        self.0.as_ref()
    }

    pub fn slice_mut(&mut self) -> &mut [Expr] {
        self.0.as_mut()
    }
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = String::new();
        for i in 0..self.0.len() {
            let n = i + 1;
            let mut s = format!("Ast::{n} => \n");
            let expr = self.0.as_ref().index(i);
            s.push_str(&format!("\t{}\nEND({n})", expr.to_string()));
            res.push_str(&s);
        }
        write!(f, "{}", res)
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Call(Rc<[Expr]>),
    Block(Rc<[Expr]>),

    // DoBlock(),
    List(Rc<Expr>),
    Atom(ZValue),
}

impl Expr {
    pub fn sys_op(&self) -> Option<Op> {
        let sym = self.inner_symbol()?;
        match sym.name() {
            keywords::ADD => Some(Op::Add),
            keywords::SUB => Some(Op::Add),
            keywords::DIV => Some(Op::Div),
            keywords::MUL => Some(Op::Mul),
            keywords::CONCAT => Some(Op::Concat),
            _ => None,
        }
    }

    pub fn assignment(name: &ZSymbol, initializer: Option<&Expr>) -> Self {
        let nv = Self::Atom(ZValue::Nil);
        let init = initializer.unwrap_or(&nv);

        let ast = [
            Self::Atom(ZValue::Sym(ZSymbol::from("="))),
            Self::Atom(ZValue::Sym(name.clone())),
            init.clone(),
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

fn expr_fmt(e: &Expr) -> String {
    match e {
        Expr::Call(ce) => {
            let mut s = String::new();
            s.push_str("(");

            for i in ce.iter() {
                s.push_str(&format!("{} ", expr_fmt(i)))
            }
            format!("{})", s.trim_end())
        }
        Expr::Block(bl) => {
            let mut s = String::new();
            s.push_str("(do\n");
            for expr in bl.iter() {
                s.push_str(&format!("{}\n", expr_fmt(expr)));
            }
            s.push_str(" end)\n");
            s
        }
        Expr::List(gr) => {
            let s = expr_fmt(gr.as_ref());
            format!("({})", s)
        }
        Expr::Atom(at) => at.to_string(),
    }
    // s.push_str("( ")
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = expr_fmt(self);
        write!(f, "{s}")
    }
}

#[derive(Error, Clone, Debug)]
pub enum AstError {
    #[error("Runtime Error :: {token:?} => {message}")]
    RuntimeError { token: LexTok, message: String },
    #[error("Type Error :: {value:?} => {message}")]
    TypeError { value: ZValue, message: String },
    #[error("Parse Error :: {token:?} - {message} AST => {expr:?}")]
    ParseError {
        token: LexTok,
        message: String,
        expr: Option<Expr>,
    },
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
