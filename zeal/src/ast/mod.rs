pub mod lex;
pub mod parse;

use crate::ast::lex::{LexTok, Tok, TokBuffer};
use crate::ast::parse::Parser;
use crate::compiler::opcode::Op;
use crate::core_types::str::ZString;
use crate::core_types::vec::ZVec;
use crate::core_types::{idents, Meta};
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::{fmt::Display, ops::Index, rc::Rc};

use thiserror::Error;

use crate::core_types::{str::ZIdent, val::ZValue};

#[derive(Debug, Clone)]
pub struct SymbolTable(HashMap<ZValue, Meta>);

impl SymbolTable {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
}

impl Deref for SymbolTable {
    type Target = HashMap<ZValue, Meta>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for SymbolTable {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum VarType {
    Let,
    Var,
    Const,
}

impl VarType {
    pub fn ident(self) -> ZIdent {
        match self {
            VarType::Let => ZIdent::new(idents::LET),

            VarType::Var => ZIdent::new(idents::VAR),
            VarType::Const => ZIdent::new(idents::CONST),
        }
    }
}

pub trait AstPass {
    fn pipe(&self, ast: Ast) -> anyhow::Result<Ast>;
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub tree: Box<[Expr]>,
    pub symbols: SymbolTable,
}

impl Ast {
    pub fn pipe<Pass>(self, pass: &mut Pass) -> anyhow::Result<Self>
    where
        Pass: AstPass,
    {
        pass.pipe(self)
    }

    pub fn empty() -> Self {
        let vec: Vec<Expr> = vec![];
        Self {
            tree: vec.into_boxed_slice(),
            symbols: SymbolTable::new(),
        }
    }

    pub fn from_toks(toks: &[Tok]) -> anyhow::Result<Self> {
        let ast = Parser::parse_ast(toks)?;
        let ast = ast.into_boxed_slice();
        let symbols = Self::build_symbol_table(ast.as_ref());
        let s = Self { tree: ast, symbols };
        Ok(s)
        // Ok(Self(ast))
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
        let stree = self.tree.as_ref();
        let other_tree = other.tree.as_ref();
        let tree_res = ([stree, other_tree]).concat().into_boxed_slice();

        let syms_res = {
            let mut sres = self.symbols.0.clone();
            let mut other_syms = other.symbols.clone();
            sres.extend(other_syms.drain());

            sres
        };

        Self {
            tree: tree_res,
            symbols: SymbolTable(syms_res),
        }
    }

    #[inline]
    pub fn append(&mut self, other: Self) {
        *self = self.concat(&other);

        // self.tree = ([stree, other_tree]).concat().into_boxed_slice();
        // self.symbols =
    }

    // pub fn to_list(&self) -> Vec<ZValue> {
    //     let mut list = Vec::new();
    //
    //     for ast in self.0.iter() {
    //         match ast {
    //             Expr::Call(_) => todo!(),
    //             Expr::Block(_) => todo!(),
    //             Expr::List(_) => todo!(),
    //             Expr::Atom(_) => todo!(),
    //             Expr::Effect(_) => todo!(),
    //         }
    //     }
    //
    //     list
    // }

    fn values(ast: &Expr, st: &mut SymbolTable) {
        match ast {
            Expr::Call(e) | Expr::Block(e) => {
                for expr in e.iter() {
                    Self::values(expr, st);
                }
            }
            Expr::Effect(e) => if let Some(Expr::Atom(ZValue::Ident(head))) = e.first() {},
            Expr::Atom(a) => {
                if a.is_ident() {
                    st.insert(a.clone(), Meta::new());
                }
            }
        }
    }

    fn build_symbol_table(ast: &[Expr]) -> SymbolTable {
        let mut st = SymbolTable::new();
        for expr in ast.iter() {
            Self::values(expr, &mut st);
        }
        st
    }
}

// /// 1:1 to Expr tree. But as a simple s-expression representation
// #[derive(Debug, Clone)]
// pub enum ValueExpr {
//     Atom(ZValue),
//     List(Rc<[ValueExpr]>),
// }
//
// impl ValueExpr {
//     pub fn to_zval(self) -> ZValue {
//         match self {
//             ValueExpr::Atom(zv) => zv,
//             ValueExpr::List(li) => {
//                 for ve in li {}
//             }
//         }
//     }
//
//     pub fn inner_zval(&self) -> ZValue {
//         match self {
//             ValueExpr::Atom(zv) => zv,
//             ValueExpr::List(li) => {
//                 for ve in li.iter() {
//                     ve.inner_zval()
//                 }
//             }
//         }
//     }
// }

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = String::new();
        for i in 0..self.tree.len() {
            let n = i + 1;
            let mut s = format!("Ast::{n} => \n");
            let expr = self.tree.as_ref().index(i);
            s.push_str(&format!("\t{}\nEND({n})", expr.to_string()));
            res.push_str(&s);
        }
        write!(f, "{}", res)
    }
}

pub type AstList<T> = Rc<[T]>;

#[derive(Debug, Clone)]
pub enum Expr {
    Call(Rc<[Expr]>),
    Block(Rc<[Expr]>),
    /// A.K.A. Statement Expression.
    Effect(Rc<[Expr]>),

    // DoBlock(),
    // List(Rc<Expr>),
    Atom(ZValue),
}

impl Expr {
    /// Reduces slice of Exprs into a single ZValue::Vec,
    /// and then wraps that in an Expr::Atom
    pub fn reduce_wrap(exprs: &[Expr]) -> Expr {
        let v = Self::reduce_slice(exprs);
        Expr::Atom(v)
    }

    /// Reduces slice of Exprs into a single ZValue::Vec,
    pub fn reduce_slice(exprs: &[Expr]) -> ZValue {
        let mut vec = Vec::with_capacity(exprs.len());
        for expr in exprs.iter() {
            let a = expr.reduce_zval();
            vec.push(a);
        }
        ZValue::Vec(ZVec::from(vec))
    }

    pub fn as_list(&self) -> Option<AstList<Expr>> {
        match self {
            Expr::Call(s) | Expr::Effect(s) | Expr::Block(s) => Some(s.clone()),
            // Expr::Call(cb) => Some(cb.clone()),
            // Expr::Block(bl) => Some(bl.clone()),
            // Expr::Effect(ef) => Some(ef.clone()),
            Expr::Atom(_) => None,
        }
    }

    pub const fn type_str(&self) -> &'static str {
        match self {
            Expr::Call(_) => "Call",
            Expr::Block(_) => "Block",
            Expr::Effect(_) => "Effect",
            Expr::Atom(_) => "Atom",
        }
    }

    /// Gets inner ZValue if variant is Atom. Otherwise panics.
    pub fn unwrap_zval(&self) -> &ZValue {
        if let Self::Atom(val) = self {
            val
        } else {
            panic!(
                "Cannot unwrap Expr as ZValue: Expected: Expr::Atom, Actual: {}",
                self.type_str()
            )
        }
    }

    /// If non-Atom variant, Reduces slice of Exprs to a Single ZValue of type ZVec.
    /// otherwise returns inner Atom ZValue.
    pub fn reduce_zval(&self) -> ZValue {
        match self {
            Expr::Call(e) | Expr::Effect(e) | Expr::Block(e) => {
                let mut vec = Vec::new();
                for expr in e.iter() {
                    let atom = expr.reduce_zval();
                    vec.push(atom.clone());
                }
                ZValue::Vec(ZVec::from(vec))
            }

            Expr::Atom(a) => a.clone(),
        }
    }

    pub fn effect(head: ZIdent, params: Option<&[Expr]>) -> Self {
        let params = if let Some(ps) = params {
            Expr::reduce_slice(ps)
        } else {
            ZValue::empty_vec()
        };
        let ast = [Self::Atom(ZValue::Ident(head)), Self::Atom(params)];
        let ast = Rc::from(ast);
        Self::Effect(ast)
    }

    pub fn assignment(name: ZIdent, rhs: Expr) -> Self {
        let ast = [
            Self::Atom(ZValue::Ident(ZIdent::from("="))),
            Self::Atom(ZValue::Ident(name)),
            rhs,
            // Self::Atom(name.clone()),
            // Self::Atom(value.clone()),
        ];
        let ast = Rc::from(ast);
        Self::Effect(ast)
    }

    pub fn is_ident_vardecl(&self) -> bool {
        if let Some(_) = self.as_ident_vardecl() {
            true
        } else {
            false
        }
    }

    pub fn as_ident_varop(&self) -> Option<ZIdent> {
        let ident = self.inner_ident()?;
        match ident.name() {
            idents::LET | idents::VAR | idents::ASSIGN => Some(ident.clone()),
            _ => None,
        }
        // if ident == || ident == reserved_idents::VAR {
        // Some(ident.clone())
        // } else {
        // None
        // }
    }

    pub fn as_ident_vardecl(&self) -> Option<ZIdent> {
        let ident = self.inner_ident()?;
        if ident == idents::LET || ident == idents::VAR {
            Some(ident.clone())
        } else {
            None
        }
    }

    pub fn binary_operator(&self) -> Option<Op> {
        let sym = self.inner_ident()?;
        match sym.name() {
            idents::ADD => Some(Op::Add),
            idents::SUB => Some(Op::Sub),
            idents::DIV => Some(Op::Div),
            idents::MUL => Some(Op::Mul),
            idents::CONCAT => Some(Op::Concat),
            idents::EQUAL => Some(Op::Eq),
            idents::NOT_EQUAL => Some(Op::NotEq),
            idents::LT => Some(Op::Lt),
            idents::GT => Some(Op::Gt),
            idents::LE => Some(Op::Le),
            idents::GE => Some(Op::Ge),
            _ => None,
        }
    }

    pub fn unary_operator(&self) -> Option<Op> {
        let sym = self.inner_ident()?;
        match sym.name() {
            idents::SUB | idents::NOT => Some(Op::Neg),
            _ => None,
        }
    }

    pub fn var_definition(name: &ZIdent, initializer: Option<&Expr>) -> Self {
        let nv = Self::Atom(ZValue::Nil);
        let init = initializer.unwrap_or(&nv);

        let ast = [
            Self::Atom(ZValue::Ident(ZIdent::from("var"))),
            Self::Atom(ZValue::Ident(name.clone())),
            init.clone(),
            // Self::Atom(name.clone()),
            // Self::Atom(value.clone()),
        ];
        let ast = Rc::from(ast);
        Self::Effect(ast)
    }

    pub fn let_definition(name: &ZIdent, initializer: Option<&Expr>) -> Self {
        let nv = Self::Atom(ZValue::Nil);
        let init = initializer.unwrap_or(&nv);

        let ast = [
            Self::Atom(ZValue::Ident(ZIdent::from("let"))),
            Self::Atom(ZValue::Ident(name.clone())),
            init.clone(),
            // Self::Atom(name.clone()),
            // Self::Atom(value.clone()),
        ];
        let ast = Rc::from(ast);
        Self::Effect(ast)
    }

    pub fn binary_op(left: &Expr, op: &ZIdent, right: &Expr) -> Self {
        let ast = [
            Self::Atom(ZValue::Ident(op.clone())),
            left.clone(),
            right.clone(),
            // Self::Atom(left.clone()),
            // Self::Atom(right.clone()),
        ];
        let ast = Rc::from(ast);
        Self::Call(ast)
    }

    pub fn unary_op(op: &ZIdent, right: &Expr) -> Self {
        let ast = [
            Self::Atom(ZValue::Ident(op.clone())),
            right.clone(),
            // Self::Atom(left.clone()),
            // Self::Atom(right.clone()),
        ];
        let ast = Rc::from(ast);
        Self::Call(ast)
    }

    pub const fn try_as_atom(&self) -> Option<&ZValue> {
        match self {
            Self::Atom(v) => Some(v),
            _ => None,
        }
    }

    pub const fn as_zval_ident(&self) -> Option<&ZValue> {
        if let Some(v) = self.try_as_atom() {
            if v.is_ident() {
                Some(v)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub const fn inner_ident(&self) -> Option<&ZIdent> {
        match self {
            Self::Atom(v) => match v {
                ZValue::Ident(s) => Some(s),
                _ => None,
            },
            _ => None,
        }
    }

    pub const fn is_ident(&self) -> bool {
        if let Some(_) = self.inner_ident() {
            true
        } else {
            false
        }
    }
}

fn expr_fmt(e: &Expr) -> String {
    match e {
        Expr::Effect(ef) => {
            let mut s = String::new();
            s.push_str("!(");

            for i in ef.iter() {
                s.push_str(&format!("{} ", expr_fmt(i)))
            }
            format!("{})", s.trim_end())
        }
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
        Expr::Atom(at) => at.to_string(),
    }
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
