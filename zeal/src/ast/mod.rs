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

use anyhow::bail;
use parse::PrintType;
use thiserror::Error;

use crate::core_types::{str::ZIdent, val::ZValue};

#[derive(Debug, Clone)]
pub struct SymbolTable(HashMap<ZIdent, Meta>);

impl SymbolTable {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn add_new(&mut self, ident: ZIdent, var_type: VarType) -> bool {
        self.add(ident, Meta::new(var_type))
    }
    /// Adds value to symbol table, Returns true if
    /// successfully added, returns false if ident is already
    /// in symbol table
    pub fn add(&mut self, ident: ZIdent, meta: Meta) -> bool {
        let k = &ident;
        if !self.contains_key(k) {
            self.insert(k.clone(), meta);
            true
        } else {
            false
        }
    }

    pub fn var_type(&self, name: &ZIdent) -> Option<VarType> {
        if let Some(meta) = self.get(name) {
            Some(meta.var_type)
        } else {
            None
        }
    }
}

impl Deref for SymbolTable {
    type Target = HashMap<ZIdent, Meta>;

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

impl Display for VarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            VarType::Let => idents::LET,
            VarType::Var => idents::VAR,
            VarType::Const => idents::CONST,
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
        }
    }
}

pub trait AstPass {
    fn pipe(&self, ast: Ast) -> anyhow::Result<Ast>;
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub tree: Vec<ExprList>,
    pub symbols: SymbolTable,
}

impl Ast {
    pub fn pipe<Pass>(self, pass: &mut Pass) -> anyhow::Result<Self>
    where
        Pass: AstPass,
    {
        pass.pipe(self)
    }

    pub fn binding_type(&self, name: &ZIdent) -> Option<VarType> {
        if let Some(meta) = self.symbols.get(name) {
            Some(meta.var_type)
        } else {
            None
        }
    }

    pub fn empty() -> Self {
        Self {
            tree: Vec::new(),
            symbols: SymbolTable::new(),
        }
    }

    pub fn from_toks(toks: &[Tok]) -> anyhow::Result<Self> {
        let tree = Parser::parse_ast(toks)?;
        let tree = Vec::from(tree);
        let symbols = Self::build_symbol_table(tree.as_ref());
        let s = Self { tree, symbols };
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
        let stree: &[ExprList] = self.tree.as_ref();
        let other_tree = other.tree.as_ref();
        let tree_res: Vec<_> = ([stree, other_tree]).concat();

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

    fn extract_binding(expr: &Expr) -> Option<(VarType, ZIdent)> {
        match expr {
            Expr::Binding { ty, name, .. } => Some((*ty, name.expect_ident())),
            _ => None,
        }
    }

    fn symbols(ast: &ExprList, st: &mut SymbolTable) {
        match ast {
            ExprList::Tuple(tup) => {
                for el in tup.iter() {
                    if let Some((ty, ident)) = Self::extract_binding(el) {
                        st.add_new(ident, ty);
                    }
                }
            }
            ExprList::Nil => {}
            ExprList::Block(bl) => {
                for el in bl.iter() {
                    Self::symbols(el, st);
                }
            }
            // ExprList::Call(cl) => {
            // st.add_new(cl.head.expect_ident());
            // }
            ExprList::Unit(ex) => match ex.as_ref() {
                Expr::Binding { ty, name, .. } => {
                    let _ = st.add_new(name.expect_ident(), *ty);
                }
                Expr::List(ex) => Self::symbols(ex, st),
                _ => {}
            },
        }
        // match ast {
        //     Expr::List(li) => match li {
        //         ExprList::Block(bl) => {
        //
        //         }
        //         ExprList::Call(cl) => {
        //             st.add_new(cl.head.expect_ident());
        //         }
        //         ExprList::Unit(ex) => Self::values(ex, st),
        //     },
        //     Expr::Call(e) | Expr::Block(e) => {
        //         for expr in e.iter() {
        //             Self::values(expr, st);
        //         }
        //     }
        //     Expr::Binding(e) => if let Some(Expr::Atom(ZValue::Ident(head))) = e.first() {},
        //     Expr::Atom(a) => {
        //         if a.is_ident() {
        //             st.insert(a.clone(), Meta::new());
        //         }
        //     }
        // }
    }

    fn build_symbol_table(ast: &[ExprList]) -> SymbolTable {
        let mut st = SymbolTable::new();
        for expr in ast.iter() {
            Self::symbols(expr, &mut st);
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
            let mut s = format!("--- AST --- \n{n} => \n");
            let expr = self.tree.as_slice().index(i);
            s.push_str(&format!("\t{}\nEND({n})", expr.to_string()));
            res.push_str(&s);
        }
        write!(f, "{}", res)
    }
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub head: ZValue,
    pub params: Rc<[Expr]>,
}

impl CallExpr {
    pub fn arity(&self) -> usize {
        self.params.len()
    }
}

pub type AstList<T> = Rc<[T]>;

#[derive(Debug, Clone, Default)]
pub enum ExprList {
    Block(AstList<Self>),

    /// List of Exprs where the first member is always some kind of function
    /// call or function-like call (+,-,/,*, ect.)
    Tuple(AstList<Expr>),
    Unit(Rc<Expr>),
    #[default]
    Nil,
}

impl Display for ExprList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = Self::fmt_exprlist(self);
        write!(f, "{}", s)
    }
}

impl ExprList {
    pub fn tuple_from_exprs(exprs: &[Expr]) -> Self {
        let mut tup = Vec::with_capacity(exprs.len());
        for ex in exprs.iter() {
            tup.push(ex.clone());
        }
        Self::Tuple(tup.into_boxed_slice().into())
    }
    pub fn expr(e: Expr) -> Self {
        Self::Unit(Rc::new(e))
    }

    pub fn tuple(vec: Vec<Expr>) -> Self {
        Self::Tuple(Rc::from(vec.into_boxed_slice()))
    }

    pub fn atom(v: ZValue) -> Self {
        Self::Unit(Rc::new(Expr::Atom(v)))
    }

    fn fmt_exprlist(el: &Self) -> String {
        match el {
            ExprList::Block(bl) => {
                let mut s = String::from("(do");
                for ex in bl.iter() {
                    let expr_s = Self::fmt_exprlist(ex);
                    s.push_str(&format!(" {}", expr_s));
                }
                s.push_str(&format!(" end)"));
                s
            }
            // ExprList::Call(cl) => {
            //     let mut s = format!("({}", cl.head.expect_ident().name());
            //     for ex in cl.params.iter() {
            //         let expr_s = Self::fmt_exprlist(ex);
            //         s.push_str(&format!(" {}", expr_s));
            //     }
            //     s.push_str(&format!(" )"));
            //     s
            // }
            ExprList::Tuple(tup) => {
                let mut s = String::from("[");
                for ex in tup.iter() {
                    let expr_s = expr_fmt(ex);
                    s.push_str(&format!(" {}", expr_s));
                }
                s.push_str(&format!("]"));
                s
            }
            ExprList::Unit(ex) => ex.to_string(),
            ExprList::Nil => "nil".into(),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub enum Expr {
    Binding {
        ty: VarType,
        /// Must be ZValue::Ident
        name: ZValue,
        init: Option<ExprList>,
    },
    Call(CallExpr),
    List(ExprList),
    Atom(ZValue),
    #[default]
    Nil,
}

impl Expr {
    // /// Reduces slice of Exprs into a single ZValue::Vec,
    // /// and then wraps that in an Expr::Atom
    // pub fn reduce_wrap(exprs: &[Expr]) -> Expr {
    //     let v = Self::reduce_slice(exprs);
    //     Expr::Atom(v)
    // }
    //
    // /// Reduces slice of Exprs into a single ZValue::Vec,
    // pub fn reduce_slice(exprs: &[Expr]) -> ZValue {
    //     let mut vec = Vec::with_capacity(exprs.len());
    //     for expr in exprs.iter() {
    //         let a = expr.reduce_zval();
    //         vec.push(a);
    //     }
    //     ZValue::Vec(ZVec::from(vec))
    // }
    //
    // pub fn as_expr_list(&self) -> Option<CallList> {
    //     match self {
    //         Expr::Call(s) | Expr::Binding(s) | Expr::Block(s) => Some(s.clone()),
    //         // Expr::Call(cb) => Some(cb.clone()),
    //         // Expr::Block(bl) => Some(bl.clone()),
    //         // Expr::Effect(ef) => Some(ef.clone()),
    //         Expr::List(_) => None,
    //         Expr::Atom(_) => None,
    //     }
    // }

    #[inline]
    pub fn to_unit_list(&self) -> ExprList {
        ExprList::Unit(Rc::new(self.clone()))
    }

    pub const fn type_str(&self) -> &'static str {
        match self {
            Expr::Binding { .. } => "Binding",
            Expr::List(_) => "List",
            Expr::Atom(_) => "Atom",
            Expr::Nil => "Nil",
            Expr::Call(_) => "Call",
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
    // pub fn reduce_zval(&self) -> ZValue {
    //     match self {
    //         Expr::Call(e) | Expr::Effect(e) | Expr::Block(e) => {
    //             let mut vec = Vec::new();
    //             for expr in e.iter() {
    //                 let atom = expr.reduce_zval();
    //                 vec.push(atom.clone());
    //             }
    //             ZValue::Vec(ZVec::from(vec))
    //         }
    //
    //         Expr::Atom(a) => a.clone(),
    //         Expr::List(_) => todo!(),
    //     }
    // }

    pub fn binding(ty: VarType, head: ZIdent, init: Option<ExprList>) -> Self {
        Self::Binding {
            ty,
            name: ZValue::Ident(head),
            init,
        }
    }

    pub fn assignment(name: ZIdent, rhs: Expr) -> Self {
        let head = ZValue::Ident(ZIdent::from("="));
        let params = [Expr::Atom(ZValue::Ident(name)), rhs];
        let params = Rc::from(params);
        let cl = CallExpr { head, params };
        Self::Call(cl)
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

    pub fn var_definition(name: ZIdent, init: Option<ExprList>) -> Self {
        Self::binding(VarType::Var, name, init)
        // let nv = Self::Atom(ZValue::Nil);
        // let init = initializer.unwrap_or(&nv);
        //
        // let ast = [
        //     Self::Atom(ZValue::Ident(ZIdent::from("var"))),
        //     Self::Atom(ZValue::Ident(name.clone())),
        //     init.clone(),
        //     // Self::Atom(name.clone()),
        //     // Self::Atom(value.clone()),
        // ];
        // let ast = Rc::from(ast);
        // Self::Binding {
        //     ty:
        //     name: todo!(),
        //     init: todo!(),
        // }
    }

    pub fn let_definition(name: ZIdent, init: Option<ExprList>) -> Self {
        Self::binding(VarType::Let, name, init)
        // let nv = Self::Atom(ZValue::Nil);
        // let init = initializer.unwrap_or(&nv);
        //
        // let ast = [
        //     Self::Atom(ZValue::Ident(ZIdent::from("let"))),
        //     Self::Atom(ZValue::Ident(name.clone())),
        //     init.clone(),
        //     // Self::Atom(name.clone()),
        //     // Self::Atom(value.clone()),
        // ];
        // let ast = Rc::from(ast);
        // Self::Binding(ast)
    }

    pub fn binary_op(left: &Expr, op: &ZIdent, right: &Expr) -> Self {
        let ast = [
            left.clone(),
            right.clone(),
            // Self::Atom(left.clone()),
            // Self::Atom(right.clone()),
        ];
        let cl = CallExpr {
            head: ZValue::Ident(op.clone()),
            params: Rc::from(ast),
        };
        Self::Call(cl)
    }

    pub fn unary_op(op: &ZIdent, right: &Expr) -> Self {
        let head = ZValue::Ident(op.clone());
        let ast = [
            right.clone(),
            // Self::Atom(left.clone()),
            // Self::Atom(right.clone()),
        ];
        let params = Rc::from(ast);
        let cl = CallExpr { head, params };
        Self::Call(cl)
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
        Expr::Call(ce) => {
            let mut s = format!("({}", ce.head.expect_ident().name());

            for i in ce.params.iter() {
                s.push_str(&format!("{} ", expr_fmt(i)))
            }
            format!("{})", s.trim_end())
        }
        Expr::List(li) => {
            li.to_string()
            // let mut s = String::new();
            // s.push_str("[");
            //
            // for i in li.iter() {
            //     s.push_str(&format!("{} ", i.to_string()))
            // }
            // format!("{}]", s.trim_end())
        }
        // Expr::Block(bl) => {
        //     let mut s = String::new();
        //     s.push_str("(do\n");
        //     for expr in bl.iter() {
        //         s.push_str(&format!("{}\n", expr_fmt(expr)));
        //     }
        //     s.push_str(" end)\n");
        //     s
        // }
        Expr::Atom(at) => at.to_string(),
        Expr::Binding { ty, name, init } => {
            let init = if let Some(i) = init.as_ref() {
                i.clone()
            } else {
                ExprList::Nil
            };

            format!("({} {name}, {})", ty.to_string(), init,)
        }
        Expr::Nil => todo!(),
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
        expr: Option<ExprList>,
    },
}
