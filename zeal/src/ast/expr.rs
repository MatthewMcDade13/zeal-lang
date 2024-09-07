use std::{fmt::Display, rc::Rc};

use crate::core_types::{
    idents,
    str::{ZIdent, ZString},
    val::ZValue,
};

use super::{BinaryOpType, UnaryOpType, VarType};

#[derive(Debug, Default, Clone)]
pub enum LoopExpr {
    #[default]
    Loop,
    /// (while cond_expr [...while_body])
    While { cond: Rc<Expr> },

    /// (each [loop_ident iterable|range?] [...each_body])
    Each {
        iter_ident: ZIdent,
        iterable: ZValue,
    },

    /// (for init_expr cond_expr inc_expr [...for_body])
    For {
        iter_ident: ZIdent,
        range: (u32, u32),
    },
}

#[derive(Debug, Clone)]
pub enum WhenForm {
    Branch(Expr, Expr),
    Else(Expr),
    End,
    // pub body: ExprList,
}

impl From<(Expr, Expr)> for WhenForm {
    fn from(value: (Expr, Expr)) -> Self {
        match value {
            (Expr::Nil, Expr::Nil) => Self::End,
            (Expr::Nil, block) => Self::Else(block),
            (cond, block) => Self::Branch(cond, block),
        }
    }
}

impl WhenForm {
    pub const fn is_end(&self) -> bool {
        match self {
            Self::End => true,
            _ => false,
        }
    }

    #[inline]
    pub fn as_tup(&self) -> (&Expr, &Expr) {
        match self {
            WhenForm::Branch(c, b) => (c, b),
            WhenForm::Else(b) => (&Expr::Nil, b),
            WhenForm::End => (&Expr::Nil, &Expr::Nil),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FormExpr {
    /// (if cond_expr [...if_body] [...else_body])
    When {
        conds: AstList<WhenForm>,
        // if_body: ExprList,
        // else_body: Option<Rc<Expr>>,
    },

    /// (break expr?)
    Break {
        expr: Option<Rc<Expr>>,
    },

    /// (loop [...body])
    Loop {
        body: ExprList,
        meta: LoopExpr,
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
        let mut state = fmt::State::default();
        let s = fmt::exprlist(self, &mut state);
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

    /// Clones self into a new Expr::List
    #[inline]
    pub fn as_expr(&self) -> Expr {
        self.clone().into_expr()
    }

    pub const fn into_expr(self) -> Expr {
        Expr::List(self)
    }

    #[inline]
    pub fn block(exs: Vec<Expr>) -> Self {
        Self::Block(exs.into_boxed_slice().into())
    }

    #[inline]
    pub fn tuple(vec: Vec<Expr>) -> Self {
        Self::Tuple(Rc::from(vec.into_boxed_slice()))
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
    pub fn stringify<T>(item: &T) -> Self
    where
        T: Display,
    {
        let s = item.to_string();
        Self::Atom(ZValue::string(ZString::new(&s)))
    }

    pub fn string_tuple<T>(items: &[T]) -> Self
    where
        T: Display,
    {
        let mut xs = Vec::new();
        for i in items.iter() {
            let x = Self::stringify(i);
            xs.push(x);
        }
        Self::List(ExprList::tuple(xs))
    }

    pub const fn break_form(expr: Option<Rc<Expr>>) -> Self {
        Self::Form(FormExpr::Break { expr })
    }
    pub const fn loop_form(body: ExprList, meta: LoopExpr) -> Self {
        Self::Form(FormExpr::Loop { body, meta })
    }

    pub fn when_form(conds: Vec<WhenForm>) -> Self {
        let conds = conds.into_boxed_slice();
        let conds = Rc::from(conds);
        Self::Form(FormExpr::When { conds })
    }

    pub const fn is_if_form(&self) -> bool {
        match self {
            Expr::Form(FormExpr::When { .. }) => true,
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

    /// Attempts to put self into an ExprList. If self is Expr::List,
    /// conversion is just a cast, otherwise converts self properties into strings and then shoves
    /// it into an ExprList::Tuple
    pub fn into_tuple(self) -> ExprList {
        match self {
            Expr::Binding { ty, name, init } => {
                let t = Expr::stringify(&ty);
                let n = Expr::stringify(&name);
                let i = Expr::clone(init.as_ref());
                ExprList::Tuple([t, n, i].into())
            }
            Expr::Form(_) => todo!(),
            Expr::List(_) => todo!(),
            Expr::Atom(_) => todo!(),
            Expr::Nil => todo!(),
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

mod fmt {
    use std::{any::Any, fmt::Debug};

    use super::*;

    #[derive(Debug, Default, Clone, Copy)]
    pub struct State {
        pub indent: usize,
        pub line: usize,
    }

    impl State {
        fn fmtln(&mut self, line_str: &str) -> String {
            let pad = "\t".repeat(self.indent);
            self.line += 1;
            format!("{pad}{line_str}\n" /*, self.line - 1*/)
        }

        fn writeln(&mut self, s: &str) -> String {
            self.line += 1;
            format!("{s}\n")
        }

        #[inline]
        fn inc_indent(&mut self) {
            self.indent += 1;
        }

        #[inline]
        fn dec_indent(&mut self) {
            self.indent -= 1;
        }
    }

    pub fn exprlist(el: &ExprList, state: &mut State) -> String {
        match el {
            ExprList::Block(bl) => {
                let mut s = state.fmtln("(block");
                state.inc_indent();
                for ex in bl.iter() {
                    let expr_s = match ex {
                        Expr::List(block @ ExprList::Block(_)) => exprlist(block, state),
                        _ => {
                            let e = expr(ex, state);
                            state.fmtln(&e)
                        }
                    };

                    s.push_str(&expr_s);
                }
                state.dec_indent();
                s.push_str(&state.fmtln("end)"));
                s
            }

            ExprList::Tuple(tup) => {
                let mut s = state.fmtln("[");
                state.inc_indent();
                for ex in tup.iter() {
                    let expr_s = expr(ex, state);
                    let expr_s = state.fmtln(&expr_s);
                    s.push_str(&expr_s);
                }
                state.dec_indent();
                s.push_str(&state.fmtln("]"));
                s
            }
            // ExprList::Unit(ex) => ex.to_string(),
            ExprList::Nil => String::from("nil"),
        }
    }

    fn value_block(x: &Expr, state: &mut State) -> String {

        match x {

            Expr::List(ExprList::Block(bl)) => {

                let mut bs = state.writeln("(block");
                state.inc_indent();
                for ex in bl.iter() {
                    let item = expr(ex, state);
                    let line = state.fmtln(&item);
                    bs.push_str(&line);
                }

                state.dec_indent();
                let end = state.fmtln("end)");
        bs.push_str(&end);
        bs
            }
            Expr::Binding { .. } => panic!("variable bindings are not allowed on right hand of when branch if not inside a block"),
            _ => expr(x, state),
            
        }



    }

    pub fn expr(e: &Expr, state: &mut State) -> String {
        match e {
            Expr::Form(ce) => match ce {
                FormExpr::When { conds } => {
                    let mut cond_str = state.writeln("(cond"); // info.fmtln("(cond");

                    state.inc_indent();

                    for cond in conds.iter() {
                        match cond.as_tup() {
                            (Expr::Nil, block) => {
                                let block_s = value_block(block, state);
                                let s = state.fmtln(&format!("_ => {block_s}"));
                                cond_str.push_str(&s);
                                break;
                            }
                            (c, block) => {
                                let cs = expr(c, state);
                                let block_s = value_block(block, state);
                                // let block_s = exprlist(block, state);
                                let s = state.fmtln(&format!("{cs} => {block_s}"));
                                // let s = state.fmtln(&s);
                                cond_str.push_str(&s);
                            }
                        }
                    }

                    state.dec_indent();
                    cond_str.push_str(&state.fmtln(&format!("end)")));

                    cond_str
                }
                FormExpr::Loop { body: ExprList::Block(bl), meta } => match meta {
                    LoopExpr::Loop => {
                        let mut ls = state.fmtln("(loop");
                        state.inc_indent();
                        for ex in bl.iter() {
                            let exs = expr(ex, state);
                            let line = state.fmtln(&exs);
                            ls.push_str(&line);
                        }
                        let end = state.fmtln("end)");
                        ls.push_str(&end);
                        
                        ls
                    } 
                    LoopExpr::While { cond } => {
                        let c = expr(cond, state);
                        let mut ls = state.fmtln(&format!("(while {c}"));
                        state.inc_indent();
                        for ex in bl.iter() {
                            let exs = expr(ex, state);
                            let line = state.fmtln(&exs);
                            ls.push_str(&line);
                        }
                        let end = state.fmtln("end)");
                        ls.push_str(&end);
                        
                        ls

                    }
                    LoopExpr::Each {
                        iter_ident,
                        iterable,
                    } => todo!(),
                    LoopExpr::For { iter_ident, range } => todo!(),
                },
                FormExpr::Loop { body, .. } => { 
                    let body = exprlist(body, state);
                    panic!("Loop body must be of type ExprList::Block. Got: {body}");
                }                FormExpr::Match {} => todo!(),
                FormExpr::Struct {} => todo!(),
                FormExpr::Impl {} => todo!(),
                FormExpr::Func {
                    name,
                    arity,
                    body,
                    last,
                } => todo!(),
                FormExpr::Print(ex) => format!("(print {})", expr(ex, state)),
                FormExpr::Println(ex) => {
                    format!("(println {})", expr(ex, state))
                }
                FormExpr::BinaryOperator { op, left, right } => {
                    let left = expr(left, state);
                    let right = expr(right, state);
                    format!("({op} {left} {right})")
                }
                FormExpr::Assign { binding, rhs } => {
                    format!("(#= {binding} {})", expr(rhs, state))
                }
                FormExpr::Call { head, params } => {
                    let mut s = state.fmtln(&format!("({} ", head.expect_ident().name()));

                    for i in params.iter() {
                        let p = format!("{} ", expr(i, state));
                        s.push_str(&p);
                    }
                    format!("{})", s.trim_end())
                }
                FormExpr::UnaryOperator { op, scalar } => todo!(),
                FormExpr::Break { expr: ex } => {
                    let s = if let Some(e) = ex {
                        format!(" {}", expr(e, state))
                    } else {
                        String::new()
                    };
                    format!("(break{s})")
                }
            },
            Expr::List(li) => exprlist(li, state),
            Expr::Atom(at) => at.to_string(),
            Expr::Binding { ty, name, init } => {
                // let init = init.as_ref() {
                //     i.clone()
                // } else {
                //     Expr::Nil
                // };

                let init_s = expr(init, state);
                format!("({} {name}, {})", ty.to_string(), init_s)
            }
            Expr::Nil => String::from("nil"),
        }
    }
}
impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        println!("matching: {:?}", self);
        let mut state = fmt::State { indent: 0, line: 1 };
        let s = fmt::expr(self, &mut state);

        write!(f, "{s}")
    }
}
