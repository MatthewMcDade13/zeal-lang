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
    pub tree: Expr,
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
            tree: Expr::Nil,
            symbols: SymbolTable::new(),
        }
    }

    pub fn from_toks(toks: &[Tok]) -> anyhow::Result<Self> {
        let tree = Parser::parse_ast(toks)?;
        let symbols = Self::build_symbol_table(&tree);
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

    // pub fn concat(&self, other: &Self) -> Self {
    //     let stree: &[Expr] = self.tree.as_ref();
    //     let other_tree = other.tree.as_ref();
    //     let tree_res: Vec<_> = ([stree, other_tree]).concat();
    //
    //     let syms_res = {
    //         let mut sres = self.symbols.0.clone();
    //         let mut other_syms = other.symbols.clone();
    //         sres.extend(other_syms.drain());
    //
    //         sres
    //     };
    //
    //     Self {
    //         tree: tree_res,
    //         symbols: SymbolTable(syms_res),
    //     }
    // }
    //
    //
    //
    // #[inline]
    // pub fn append(&mut self, other: Self) {
    //     *self = self.concat(&other);
    // }

    fn extract_binding(expr: &Expr) -> Option<(VarType, ZIdent)> {
        match expr {
            Expr::Binding { ty, name, .. } => Some((*ty, name.expect_ident())),
            _ => None,
        }
    }
    fn symbols(ast: &Expr, st: &mut SymbolTable) {
        match ast {
            Expr::Binding { ty, name, .. } => {
                let _ = st.add_new(name.expect_ident(), *ty);
            }
            Expr::List(ex) => match ex {
                ExprList::Block(bl) => {
                    for e in bl.iter() {
                        Self::symbols(e, st);
                    }
                }
                ExprList::Tuple(tup) => {
                    for e in tup.iter() {
                        Self::symbols(e, st);
                    }
                }
                // ExprList::Unit(_) => todo!(),
                ExprList::Nil => todo!(),
            }, //Self::symbols(ex, st),

            _ => {}
        }
    }

    // fn symbols(ast: &Expr, st: &mut SymbolTable) {
    //     match ast {
    //         ExprList::Tuple(tup) => {
    //             for el in tup.iter() {
    //                 if let Some((ty, ident)) = Self::extract_binding(el) {
    //                     st.add_new(ident, ty);
    //                 }
    //             }
    //         }
    //         ExprList::Nil => {}
    //         ExprList::Block(bl) => {
    //             for el in bl.iter() {
    //                 Self::symbols(el, st);
    //             }
    //         }
    //         ExprList::Unit(ex) => match ex.as_ref() {
    //             Expr::Binding { ty, name, .. } => {
    //                 let _ = st.add_new(name.expect_ident(), *ty);
    //             }
    //             Expr::List(ex) => Self::symbols(ex, st),
    //             _ => {}
    //         },
    //     }
    // }

    fn build_symbol_table(ast: &Expr) -> SymbolTable {
        let mut st = SymbolTable::new();
        Self::symbols(&ast, &mut st);
        st
    }
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = format!("----- AST ----- \n");
        if let Expr::List(ExprList::Block(l)) | Expr::List(ExprList::Tuple(l)) = &self.tree {
            for i in 0..l.len() {
                let n = i + 1;
                let expr = l.index(i);
                let s = &format!("{n} {}\n", expr.to_string());
                res.push_str(&s);
            }
        } else {
            let s = &format!("{}", self.tree.to_string());
            res.push_str(s);
        }
        res.push_str("----- END AST -----\n");
        write!(f, "{}", res)
    }
}

#[derive(Debug, Clone)]
pub enum FormExpr {
    /// (if cond_expr [...if_body] [...else_body])
    If {
        cond: Rc<Expr>,
        if_body: ExprList,
        else_body: Option<Rc<Expr>>,
    },
    /// (loop [...body])
    Loop {
        body: ExprList,
    },

    /// (while cond_expr [...while_body])
    While {
        cond: Rc<Expr>,
        body: ExprList,
    },

    /// (each [loop_ident iterable|range?] [...each_body])
    Each {
        loop_ident: ZIdent,
        iterable: Option<ZValue>,
        num_range: Option<(i32, i32)>,
    },

    /// (for init_expr cond_expr inc_expr [...for_body])
    For {
        init: Rc<Expr>,
        cond: Rc<Expr>,
        inc: Rc<Expr>,
        body: ExprList,
    },
    /// (match expr [...patterns wildcard?])
    Match {},
    /// (struct name [...fields])
    Struct {},
    /// (impl trait=self for_type [...impl_body])
    Impl {},
    /// (fn name [...body last])
    Func {
        name: ZIdent,
        arity: u32,
        body: ExprList,
        last: Rc<Expr>,
    },
    Print(Rc<Expr>),
    Println(Rc<Expr>),
    BinaryOperator {
        op: BinaryOpType,
        left: Rc<Expr>,
        right: Rc<Expr>,
    },
    UnaryOperator {
        op: UnaryOpType,
        scalar: Rc<Expr>,
    },

    Assign {
        binding: ZIdent,
        rhs: Rc<Expr>,
    },

    /// A simple call expression.
    ///     Ex:    (add 1 2)
    ///              ^  ^-^
    ///              |   |
    ///           head params
    Call {
        head: ZValue,
        params: AstList<Expr>,
    },
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
    Call,
    Negate,
    Not,
}

impl Display for UnaryOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            UnaryOpType::Call => "(...)",
            UnaryOpType::Negate => idents::SUB,
            UnaryOpType::Not => idents::NOT,
        };
        write!(f, "{s}")
    }
}

pub type AstList<T> = Rc<[T]>;

#[derive(Debug, Clone, Default)]
pub enum ExprList {
    Block(AstList<Expr>),

    Tuple(AstList<Expr>),
    // Unit(Rc<Expr>),
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
    pub fn as_slice(&self) -> Option<&[Expr]> {
        match self {
            ExprList::Block(bl) => Some(bl.as_ref()),
            ExprList::Tuple(tup) => Some(tup.as_ref()),
            ExprList::Nil => None,
        }
    }

    pub fn unwrap_tuple(&self) -> AstList<Expr> {
        match self {
            ExprList::Tuple(tup) => tup.clone(),
            _ => panic!(
                "Attempt to unwrap ExprList as tuple when variant is: {}",
                self
            ),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            ExprList::Block(bl) => bl.len(),
            ExprList::Tuple(tup) => tup.len(),
            ExprList::Nil => 0,
        }
    }

    pub fn tuple_from_slice(exprs: &[Expr]) -> Self {
        let mut tup = Vec::with_capacity(exprs.len());
        for ex in exprs.iter() {
            tup.push(ex.clone());
        }
        Self::Tuple(tup.into_boxed_slice().into())
    }
    // pub fn expr(e: Expr) -> Self {
    //     Self::Unit(Rc::new(e))
    // }

    pub fn tuple(vec: Vec<Expr>) -> Self {
        Self::Tuple(Rc::from(vec.into_boxed_slice()))
    }

    // pub fn atom(v: ZValue) -> Self {
    //     Self::Unit(Rc::new(Expr::Atom(v)))
    // }

    fn fmt_exprlist(el: &Self) -> String {
        match el {
            ExprList::Block(bl) => {
                let mut s = String::from("(block\n");
                for (i, ex) in bl.iter().enumerate() {
                    let expr_s = expr_fmt(ex);

                    s.push_str(&format!(" {}\n{}", expr_s, "\t".repeat(i)));
                }
                s.push_str(&format!(" blockend)"));
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
            // ExprList::Unit(ex) => ex.to_string(),
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
        init: Rc<Expr>,
    },
    Form(FormExpr),
    List(ExprList),
    Atom(ZValue),
    #[default]
    Nil,
}

impl Expr {
    pub const fn if_form(cond: Rc<Expr>, if_body: ExprList, else_body: Option<Rc<Expr>>) -> Self {
        Self::Form(FormExpr::If {
            cond,
            if_body,
            else_body,
        })
    }

    pub const fn is_if_form(&self) -> bool {
        match self {
            Expr::Form(FormExpr::If { .. }) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn try_list(&self) -> Option<ExprList> {
        match self {
            Expr::List(el) => Some(el.clone()),
            _ => None,
        }
    }

    #[inline]
    pub fn expect_call(&self) -> FormExpr {
        match self {
            Self::Form(ce) => ce.clone(),
            _ => panic!("Attempted to unwrap: {} as Expr::Call.", self),
        }
    }

    #[inline]
    pub fn expect_block(&self) -> ExprList {
        match self {
            Self::List(bl) => {
                if let ExprList::Block(_) = bl {
                    bl.clone()
                } else {
                    panic!("Attempted to unwrap: {} as ExprList::Block.", self)
                }
            }
            _ => panic!("Attempted to unwrap: {} as ExprList::Block.", self),
        }
    }

    #[inline]
    pub fn expect_list(&self) -> ExprList {
        match self {
            Expr::List(el) => el.clone(),
            _ => panic!("Attempted to unwrap: {} as Expr::List.", self),
        }
    }

    pub fn tuple(exprs: Vec<Expr>) -> Self {
        let e = exprs.into_boxed_slice();
        let e = Rc::from(e);
        Self::List(ExprList::Tuple(e))
    }

    pub fn block(exprs: Vec<Expr>) -> Self {
        let e = exprs.into_boxed_slice();
        let e = Rc::from(e);
        Self::List(ExprList::Block(e))
    }

    #[inline]
    pub fn block_from_slice(exprs: &[Expr]) -> Self {
        let exs = exprs.to_vec();
        Self::block(exs)
    }

    // pub fn to_unit_list(&self) -> ExprList {
    //     ExprList::Unit(Rc::new(self.clone()))
    // }
    pub const fn is_nil(&self) -> bool {
        match self {
            Expr::Nil => true,
            _ => false,
        }
    }

    pub const fn type_str(&self) -> &'static str {
        match self {
            Expr::Binding { .. } => "Binding",
            Expr::List(_) => "List",
            Expr::Atom(_) => "Atom",
            Expr::Nil => "Nil",
            Expr::Form(_) => "Call",
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

    #[inline]
    pub fn binding(ty: VarType, head: ZIdent, init: Option<Expr>) -> Self {
        Self::Binding {
            ty,
            name: ZValue::Ident(head),
            init: Rc::from(init.unwrap_or_default()),
        }
    }

    pub fn assignment(name: ZIdent, rhs: Expr) -> Self {
        let form = FormExpr::Assign {
            binding: name,
            rhs: Rc::new(rhs),
        };
        Self::Form(form)
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

    #[inline]
    pub fn var_definition(name: ZIdent, init: Option<Expr>) -> Self {
        Self::binding(VarType::Var, name, init)
    }

    #[inline]
    pub fn let_definition(name: ZIdent, init: Option<Expr>) -> Self {
        Self::binding(VarType::Let, name, init)
    }

    pub fn binary_op(left: Expr, op: ZIdent, right: Expr) -> Self {
        let op = op
            .binary_operator_type()
            .expect("Expected ident to be a valid binary operator. Got: {op}");
        let left = Rc::new(left);
        let right = Rc::new(right);
        let form = FormExpr::BinaryOperator { op, left, right };
        Self::Form(form)
    }

    pub fn unary_op(op: ZIdent, right: Expr) -> Self {
        let op = op
            .unary_operator_type()
            .expect("Expected ident to be a valid unary operator. Got: {op}");
        let scalar = Rc::new(right);
        let form = FormExpr::UnaryOperator { op, scalar };
        Self::Form(form)
        // let head = ZValue::Ident(op.clone());
        // let ast = [
        //     right.clone(),
        //     // Self::Atom(left.clone()),
        //     // Self::Atom(right.clone()),
        // ];
        // let params = Rc::from(ast);
        // // let params = Rc::new(ExprList::tuple_from_slice(&ast));
        // // let cl = CallExpr { head, params };
        // let cl = FormExpr::native(head, params);
        // Self::Form(cl)
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
        Expr::Form(ce) => match ce {
            FormExpr::If {
                cond,
                if_body,
                else_body,
            } => {
                let else_body = else_body
                    .as_ref()
                    .map(|e| format!("(else {}\n endif)", expr_fmt(e)))
                    .unwrap_or("end".into());

                format!("(if {cond} (then\n\t {if_body}\t\n {else_body}))")
            }
            FormExpr::Loop { body } => todo!(),
            FormExpr::While { cond, body } => todo!(),
            FormExpr::Each {
                loop_ident,
                iterable,
                num_range,
            } => todo!(),
            FormExpr::For {
                init,
                cond,
                inc,
                body,
            } => todo!(),
            FormExpr::Match {} => todo!(),
            FormExpr::Struct {} => todo!(),
            FormExpr::Impl {} => todo!(),
            FormExpr::Func {
                name,
                arity,
                body,
                last,
            } => todo!(),
            FormExpr::Print(expr) => format!("(print {expr})"),
            FormExpr::Println(expr) => format!("(println {expr})"),
            FormExpr::BinaryOperator { op, left, right } => format!("({op} {left} {right})"),
            FormExpr::Assign { binding, rhs } => format!("(#= {binding} {rhs})"),
            FormExpr::Call { head, params } => {
                let mut s = format!("({} ", head.expect_ident().name());

                for i in params.iter() {
                    s.push_str(&format!("{} ", expr_fmt(i)))
                }
                format!("{})", s.trim_end())
            }
            FormExpr::UnaryOperator { op, scalar } => todo!(),
        },
        Expr::List(li) => li.to_string(),
        Expr::Atom(at) => at.to_string(),
        Expr::Binding { ty, name, init } => {
            // let init = init.as_ref() {
            //     i.clone()
            // } else {
            //     Expr::Nil
            // };

            format!("({} {name}, {})", ty.to_string(), init,)
        }
        Expr::Nil => "nil".into(),
    }
}

#[inline]
fn fmt_single(head: &ZIdent, a: &Expr) -> String {
    format!("({head} {a})")
}

#[inline]
fn fmt_double(head: &ZIdent, a: &Expr, b: &Expr) -> String {
    format!("({head} {a} {b})")
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = expr_fmt(self);
        write!(f, "{s}")
    }
}
