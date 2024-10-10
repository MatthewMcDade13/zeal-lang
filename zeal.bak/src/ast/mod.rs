pub mod expr;
pub mod lex;
pub mod parse;

use crate::ast::expr::{Expr, ExprList};
use crate::ast::lex::{LexTok, Tok, TokBuffer};
use crate::ast::parse::Parser;
use crate::core_types::idents;
use crate::core_types::str::ZIdent;
use crate::meta::sym::SymTable;
use crate::meta::Meta;
use std::collections::HashMap;
use std::fmt::Display;
use std::ops::Index;
use std::ops::{Deref, DerefMut};
use std::str::FromStr;

//
// #[derive(Debug, Clone)]
// pub struct SymbolTable(HashMap<ZIdent, Meta>);
//
// impl SymbolTable {
//     pub fn new() -> Self {
//         Self(HashMap::new())
//     }
//
//     pub fn add_new(&mut self, ident: ZIdent, var_type: VarType) -> bool {
//         self.add(ident, Meta::new(var_type))
//     }
//     /// Adds value to symbol table, Returns true if
//     /// successfully added, returns false if ident is already
//     /// in symbol table
//     pub fn add(&mut self, ident: ZIdent, meta: Meta) -> bool {
//         let k = &ident;
//         if !self.contains_key(k) {
//             self.insert(k.clone(), meta);
//             true
//         } else {
//             false
//         }
//     }
//
//     pub fn var_type(&self, name: ZIdent) -> Option<VarType> {
//         self.get(&name).map(|meta| meta.var_type)
//     }
// }
//
// impl Default for SymbolTable {
//     fn default() -> Self {
//         Self::new()
//     }
// }
//
// impl Deref for SymbolTable {
//     type Target = HashMap<ZIdent, Meta>;
//
//     fn deref(&self) -> &Self::Target {
//         &self.0
//     }
// }
//
// impl DerefMut for SymbolTable {
//     fn deref_mut(&mut self) -> &mut Self::Target {
//         &mut self.0
//     }
// }

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum VarType {
    Let,
    Var,
    Const,
    Fn,
}

impl Display for VarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            VarType::Let => idents::LET,
            VarType::Var => idents::VAR,
            VarType::Const => idents::CONST,
            VarType::Fn => idents::FN,
        };
        write!(f, "{s}")
    }
}

impl VarType {
    pub fn ident(self) -> ZIdent {
        match self {
            VarType::Let => ZIdent::new(idents::LET),

            VarType::Var => ZIdent::new(idents::VAR),
            VarType::Const => ZIdent::new(idents::CONST),
            VarType::Fn => ZIdent::new(idents::FN),
        }
    }
}

pub trait AstPass {
    fn pipe(&self, ast: Ast) -> anyhow::Result<Ast>;
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub tree: Expr,
}

impl FromStr for Ast {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let buf = TokBuffer::read_string(s)?;
        let s = Self::from_toks(buf.slice())?;
        Ok(s)
    }
}
impl Ast {
    pub fn pipe<Pass>(self, pass: &mut Pass) -> anyhow::Result<Self>
    where
        Pass: AstPass,
    {
        pass.pipe(self)
    }

    pub fn empty() -> Self {
        Self { tree: Expr::Nil }
    }

    pub fn from_toks(toks: &[Tok]) -> anyhow::Result<Self> {
        let tree = Parser::parse_ast(toks)?;
        let s = Self { tree };
        Ok(s)
        // Ok(Self(ast))
    }

    // pub fn from_str(str: &str) -> anyhow::Result<Self> {
    //     let buf = TokBuffer::read_string(str)?;
    //     let s = Self::from_toks(buf.slice())?;
    //     Ok(s)
    // }

    pub fn from_file(path: &str) -> anyhow::Result<Self> {
        let buf = TokBuffer::read_file(path)?;
        let s = Self::from_toks(buf.slice())?;
        Ok(s)
    }
    //
    // fn symbols(ast: &Expr, st: &mut SymbolTable) {
    //     match ast {
    //         Expr::Binding { ty, name, .. } => {
    //             let _ = st.add_new(name.expect_ident(), *ty);
    //         }
    //         Expr::List(ex) => match ex {
    //             ExprList::Block(bl) => {
    //                 for e in bl.iter() {
    //                     Self::symbols(e, st);
    //                 }
    //             }
    //             ExprList::Tuple(tup) => {
    //                 for e in tup.iter() {
    //                     Self::symbols(e, st);
    //                 }
    //             }
    //             // ExprList::Unit(_) => todo!(),
    //             ExprList::Nil => todo!(),
    //         }, //Self::symbols(ex, st),
    //
    //         _ => {}
    //     }
    // }
    //
    //     fn build_symbol_table(ast: &Expr) -> SymbolTable {
    //         let mut st = SymbolTable::new();
    //         Self::symbols(&ast, &mut st);
    //         st
    //     }
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = format!("----- AST ----- \n");
        res.push_str(&self.tree.to_string());
        // if let Expr::List(ExprList::Block(l)) | Expr::List(ExprList::Tuple(l)) = &self.tree {
        //     for i in 0..l.len() {
        //         let expr = l.index(i);
        //         res.push_str(&expr.to_string());
        //     }
        // } else {
        //     let s = &format!("{}", self.tree.to_string());
        //     res.push_str(s);
        // }
        res.push_str("----- END AST -----\n");
        write!(f, "{}", res)
    }
}
#[derive(Debug, Clone, Copy)]
pub enum BinaryOpType {
    Gt,
    Lt,
    Ge,
    Le,
    And,
    Or,
    Equals,
    NotEquals,
    BitAnd,
    BitOr,
    Xor,
    Concat,
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for BinaryOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            BinaryOpType::Gt => idents::GT,
            BinaryOpType::Lt => idents::LT,
            BinaryOpType::Ge => idents::GE,
            BinaryOpType::Le => idents::LE,
            BinaryOpType::And => idents::AND,
            BinaryOpType::Or => idents::OR,
            BinaryOpType::Equals => idents::EQUAL,
            BinaryOpType::NotEquals => idents::NOT_EQUAL,
            BinaryOpType::BitAnd => todo!(),
            BinaryOpType::BitOr => todo!(),
            BinaryOpType::Xor => todo!(),
            BinaryOpType::Concat => idents::CONCAT,
            BinaryOpType::Add => idents::ADD,
            BinaryOpType::Sub => idents::SUB,
            BinaryOpType::Mul => idents::MUL,
            BinaryOpType::Div => idents::DIV,
        };
        write!(f, "{s}")
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOpType {
    Negate,
    Not,
}

impl Display for UnaryOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            UnaryOpType::Negate => idents::SUB,
            UnaryOpType::Not => idents::NOT,
        };
        write!(f, "{s}")
    }
}
