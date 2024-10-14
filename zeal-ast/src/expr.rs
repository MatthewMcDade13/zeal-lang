use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
    rc::Rc,
    slice::Iter,
};

use anyhow::bail;

use crate::{AstStringify, AstWalker};

pub type AstRune = Rc<str>;
pub type ExprNode = Rc<Expr>;

#[derive(Debug, Clone)]
pub enum AstList<T> {
    Nil,
    List(Rc<[T]>),
}

impl<T> AstList<T> {
    pub fn new(list: Vec<T>) -> Self {
        let l = list.into_boxed_slice();
        Self::List(Rc::from(l))
    }

    pub fn from_slice(list: &[T]) -> Self
    where
        T: Clone,
    {
        let l = list.to_vec();
        Self::new(l)
    }

    pub fn iter(&self) -> anyhow::Result<Iter<'_, T>> {
        if let AstList::List(l) = &self {
            Ok(l.iter())
        } else {
            bail!("Cannot create iterator from a nil AstList!!!")
        }
    }

    pub fn try_get(&self) -> Option<&Rc<[T]>> {
        match self {
            AstList::Nil => None,
            AstList::List(rc) => Some(rc),
        }
    }

    pub fn unwrap(&self) -> &Rc<[T]> {
        match self {
            AstList::Nil => panic!("Attempted to unwrap a nil AstList!!!"),
            AstList::List(rc) => rc,
        }
    }

    pub const fn is_empty(&self) -> bool {
        matches!(self, Self::Nil)
    }

    pub fn len(&self) -> usize {
        match self {
            AstList::Nil => 0,
            AstList::List(rc) => rc.len(),
        }
    }

    pub fn to_vec(self) -> Vec<T>
    where
        T: Clone,
    {
        match self {
            AstList::Nil => Vec::new(),
            AstList::List(rc) => rc.to_vec(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum WhenForm {
    Branch(Expr, ExprStmt),
    Else(ExprStmt),
    End,
    // pub body: ExprList,
}

impl From<(Expr, ExprStmt)> for WhenForm {
    fn from(value: (Expr, ExprStmt)) -> Self {
        match value {
            (Expr::Unit, ExprStmt::Atom(Expr::Unit)) => Self::End,
            (Expr::Unit, block) => Self::Else(block),
            (cond, block) => Self::Branch(cond, block),
        }
    }
}

impl WhenForm {
    pub fn branch(cond: Expr, body: Vec<ExprStmt>) -> Self {
        if body.len() == 1 {
            let rhs = body.first().unwrap().clone();
            Self::Branch(cond, rhs)
        } else {
            let body = AstList::new(body);
            let body = ExprStmt::Block(body);
            Self::Branch(cond, body)
        }
    }
    pub const fn is_end(&self) -> bool {
        matches!(self, Self::End)
    }

    #[inline]
    pub fn as_tup(&self) -> (&Expr, &ExprStmt) {
        match self {
            WhenForm::Branch(c, b) => (c, b),
            WhenForm::Else(b) => (&Expr::Unit, b),
            WhenForm::End => (&Expr::Unit, &ExprStmt::Atom(Expr::Unit)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub name: AstRune,
    pub typename: Option<AstRune>,
}

impl Binding {
    pub const fn typed(name: AstRune, typename: AstRune) -> Self {
        let typename = Some(typename);
        Self { name, typename }
    }

    pub const fn new(name: AstRune) -> Self {
        Self {
            name,
            typename: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: AstRune,
    pub params: AstList<Binding>,
    pub body: Rc<ExprStmt>,
}

impl FuncDecl {
    pub fn arity(&self) -> usize {
        if let AstList::List(ps) = &self.params {
            ps.len()
        } else {
            0
        }
    }
}
#[derive(Debug, Clone, Copy, Default)]
pub enum BindType {
    #[default]
    Let,
    Var,
    Const,
    Function,
    Struct,
    Trait,
    Newtype,
}

#[derive(Debug, Clone)]
pub struct BindStmt {
    pub name: AstRune,
    pub bind_type: BindType,
    pub rhs: Expr,
}

impl Display for BindStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bt = match self.bind_type {
            BindType::Let => "let",
            BindType::Var => "var",
            BindType::Const => "const",
            BindType::Function => "fn",
            BindType::Struct => "struct",
            BindType::Trait => "trait",
            BindType::Newtype => "newtype",
        };
        if let Ok(rhs) = AstStringify::expr_tostring(&self.rhs) {
            let s = format!("({bt} {} {rhs})", self.name.as_ref());
            write!(f, "{s}")
        } else {
            std::fmt::Result::Err(std::fmt::Error)
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExprStmt {
    Block(AstList<Self>),
    Loop(AstList<Self>),
    While { cond: Expr, body: AstList<Self> },
    When(AstList<WhenForm>),
    DefFunc(FuncDecl),
    Binding(BindStmt),
    Escape(EscapeExpr),
    Atom(Expr),
}

impl ExprStmt {
    #[inline]
    pub fn type_str(&self) -> String {
        let s = match self {
            ExprStmt::Block(_) => "Block",
            ExprStmt::Loop(_) => "Loop",
            ExprStmt::While { .. } => "While",
            ExprStmt::When(_) => "When",
            ExprStmt::DefFunc(_) => "DefFunc",
            ExprStmt::Binding(_) => "Binding",
            ExprStmt::Escape(_) => "Escape",
            ExprStmt::Atom(_) => "AtomExpr",
        };
        String::from(s)
    }
    pub fn into_atom_expr(self) -> Expr {
        match self {
            Self::Atom(e) => e,
            _ => panic!("Cannot cast ExprStmt into Atom Expr!, Got: {:?}", self),
        }
    }

    pub const fn continue_stmt() -> Self {
        Self::Escape(EscapeExpr::Continue)
    }

    pub const fn break_stmt(rhs: Expr) -> Self {
        Self::Escape(EscapeExpr::Break(rhs))
    }

    pub const fn return_stmt(rhs: Expr) -> Self {
        Self::Escape(EscapeExpr::Return(rhs))
    }

    pub const fn binding(name: AstRune, rhs: Expr, bind_type: BindType) -> Self {
        let b = BindStmt {
            name,
            bind_type,
            rhs,
        };
        Self::Binding(b)
    }
    pub const fn unit() -> Self {
        Self::Atom(Expr::Unit)
    }

    pub fn while_block(cond: Expr, body: Vec<Self>) -> Self {
        let body = AstList::new(body);

        Self::While { cond, body }
    }

    /// Same as Self::block, but if give body vec is of length == 1,
    /// then the block stmt 'becomes' its only expresion statement.
    #[inline]
    pub fn block_elide(body: Vec<Self>) -> Self {
        if body.len() == 1 {
            body.first().unwrap().clone()
        } else {
            Self::block(body)
        }
    }

    #[inline]
    pub fn block(body: Vec<Self>) -> Self {
        let list = AstList::new(body);
        Self::Block(list)
    }

    pub fn when_block(conds: Vec<WhenForm>) -> Self {
        let cs = AstList::new(conds);
        Self::When(cs)
    }

    pub fn call(head: Expr, args: Vec<Expr>) -> Self {
        let e = Expr::call_expr(head, args);
        Self::Atom(e)
    }

    pub fn func_decl(name: AstRune, params: AstList<Binding>, body: ExprStmt) -> Self {
        assert!(
            matches!(body, ExprStmt::Block(_)),
            "Function must have a body that is ExprStmt::Block(..). Got: {:?}",
            body
        );
        let body = Rc::new(body);
        let f = FuncDecl { name, params, body };
        Self::DefFunc(f)
    }

    pub fn loop_block(body: Vec<ExprStmt>) -> Self {
        let bl = AstList::new(body);
        Self::Loop(bl)
    }
}

//

#[derive(Debug, Clone)]
pub enum Expr {
    Rune(AstRune),
    Bool(bool),
    // RuneWord(Rc<[(AstRune, ExprNode)]>),
    Byte(u8),
    SByte(i8),
    Int(isize),
    Uint(usize),
    Float(f64),
    String(Rc<str>),
    List(AstList<Self>),
    Pair([Rc<Expr>; 2]),
    Triple([Rc<Expr>; 3]),
    Assign {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    /// args Expr must be Self::List | Self::Pair | Self::Triple!!!!
    Call {
        head: Rc<Expr>,
        args: Rc<Expr>,
    },

    Unit,
}

#[derive(Debug, Clone)]
pub enum EscapeExpr {
    Return(Expr),
    Break(Expr),
    Continue,
}

impl Display for EscapeExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            EscapeExpr::Return(expr) => {
                if let Ok(es) = AstStringify::expr_tostring(expr) {
                    format!("(return {es})")
                } else {
                    return std::fmt::Result::Err(std::fmt::Error);
                }
            }
            EscapeExpr::Break(expr) => {
                if let Ok(es) = AstStringify::expr_tostring(expr) {
                    format!("(break {es}")
                } else {
                    return std::fmt::Result::Err(std::fmt::Error);
                }
            }

            EscapeExpr::Continue => String::from("(continue ())"),
        };
        write!(f, "{s}")
    }
}

impl Expr {
    pub const UNIT: Self = const { Self::Unit };

    pub const fn type_str(&self) -> &'static str {
        match self {
            Expr::Rune(_) => "Rune",
            Expr::Bool(_) => "Bool",
            Expr::Int(_) => "Integer",
            Expr::Uint(_) => "Unsigned Integer",
            Expr::Float(_) => "Float64",
            Expr::String(_) => "String",
            Expr::List(_) => "List",
            Expr::Pair(_) => "Pair",
            Expr::Triple(_) => "Triple",
            Expr::Assign { .. } => "Assign",
            Expr::Call { .. } => "Call",
            Expr::Unit => "()",
            Expr::Byte(_) => "Unsigned Byte",
            Expr::SByte(_) => "Signed Byte",
        }
    }

    #[inline]
    pub fn assignment(lhs: Expr, rhs: Expr) -> Self {
        let lhs = Rc::new(lhs);
        let rhs = Rc::new(rhs);
        Self::Assign { lhs, rhs }
    }

    #[inline]
    pub fn unary_op(op: Expr, rhs: Expr) -> Self {
        Self::call_slice(op, &[rhs])
    }

    #[inline]
    pub fn binary_op(lhs: Expr, op: Expr, rhs: Expr) -> Self {
        Self::call_slice(op, &[lhs, rhs])
    }

    pub fn list(list: Vec<Self>) -> Self {
        let list = AstList::new(list);
        Self::List(list)
    }

    pub fn pair(a: Self, b: Self) -> Self {
        Self::Pair([Rc::new(a), Rc::new(b)])
    }

    #[inline]
    pub fn triple(a: Self, b: Self, c: Self) -> Self {
        Self::Triple([Rc::new(a), Rc::new(b), Rc::new(c)])
    }

    #[inline]
    pub fn call_slice(head: Expr, args: &[Self]) -> Self {
        Self::call(head, args.to_vec())
    }

    pub fn call(head: Expr, args: Vec<Self>) -> Self {
        let head = Rc::new(head);
        let args = Self::args_to_expr(args);
        let args = Rc::new(args);
        Self::Call { head, args }
    }

    #[inline]
    pub fn call_expr_slice(head: Self, args: &[Self]) -> Self {
        Self::call_expr(head, args.to_vec())
    }

    pub fn call_expr(head: Self, args: Vec<Self>) -> Self {
        let head = Rc::new(head);
        let args = Self::args_to_expr(args);
        let args = Rc::new(args);
        Self::Call { head, args }
    }

    pub const fn unit_pair() -> (Self, Self) {
        (Self::Unit, Self::Unit)
    }

    pub fn rune(name: &str) -> Self {
        let r = Rc::from(name);
        Self::Rune(r)
    }

    fn args_to_expr(args: Vec<Self>) -> Self {
        match args.len() {
            0 => Self::Unit,
            1 => args.first().unwrap().clone(),
            2 => {
                let a = args.first().unwrap().clone();
                let b = args.get(1).unwrap_or(&Expr::Unit);
                Self::pair(a, b.clone())
            }
            3 => {
                let a = args.first().unwrap().clone();
                let b = args.get(1).unwrap().clone();
                let c = args.get(2).unwrap().clone();
                Self::triple(a, b, c)
            }
            _ => Self::list(args),
        }
    }
}
