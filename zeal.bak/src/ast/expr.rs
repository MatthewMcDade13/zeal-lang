use std::{fmt::Display, rc::Rc};

use crate::core_types::{
    idents,
    str::{ZIdent, ZString},
    val::ZValue,
};

use super::{BinaryOpType, UnaryOpType, VarType};

pub type ExprNode = Rc<Expr>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Atom(ZValue);

pub enum Tuple {
    Double(ExprNode, ExprNode),
    Triple(ExprNode, ExprNode, ExprNode),
}

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



// #[derive(Default, Debug, Clone, Copy)]
// pub enum Namespace {
//     #[default]
//     Script, 
//     Module, 
//     Function, 
//     Impl 
// }
//
// #[derive(Debug, Clone)]
// pub struct FuncForm {
//     pub name: ZIdent,
//     pub args: Option<AstList<ZIdent>>, 
//     pub body: ExprList,
//     pub namespace: Namespace,
// }

// #[derive(Debug, Clone)]
// pub struct FuncForm {
//     pub name: ZIdent, 
//     pub params: Option<AstList<ZIdent>>, 
//     pub body: ExprList
// }
//

impl FuncExpr {
    #[inline]
    pub fn arity(&self) -> usize {
        if let Some(ps) = self.args.as_ref() {
            ps.len()
        } else {
            0
        }
    } 

    // pub const fn is_module(&self) -> bool {
    //     matches!(self, Self::Module { .. })
    // }

    // pub const fn is_func(&self) -> bool {
    //     matches!(self, Self::Function { .. })
    // }

    // pub const fn params(&self) -> Option<&AstList<ZIdent>> {
    //         self.params.as_ref() 
    // }
    //

    // pub fn name(&self) -> &str {
    //     match self {
    //         FuncForm::Module { .. } => "<<Module>>", 
    //         FuncForm::Function { name, .. } => name.string(),
    //     }
    // }

    pub fn name_ident(&self) -> ZIdent {
        self.name.clone()
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

    /// (return expr?)
    Return {
        expr: Option<Rc<Expr>>,
    },

    /// (match expr [...patterns wildcard?])
    Match {},
    /// (struct name [...fields])
    Struct {},
    /// (impl trait=self for_type [...impl_body])
    Impl {},
    /// (fn name [...body last])
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
        callee: Rc<Expr>,
        // head: ZValue,
        params: Option<AstList<Expr>>,
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
    /// converts Self::Block | Self::Tuple with exactly 1 element to its single elements Expr Type
    pub fn try_unwrap_single(&self) -> Option<Expr> {
        match self {
            ExprList::Block(bl) if bl.len() == 1 => Some(bl[0].clone()), 
            ExprList::Tuple(tup) if tup.len() == 1 => Some(tup[0].clone()), 
            ExprList::Nil => None, 
            _ => None
        }
    }
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

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
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
        self.clone().into_expr_list()
    }

    /// "Upgrades" ExprList to a Expr::List.
    /// self.into_expr_list() == Expr::List(self)
    pub const fn into_expr_list(self) -> Expr {
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

#[derive(Debug, Clone)]
pub enum NewtypeExpr {
    Function(FuncDecl),
    Alias,
    Wrap,
    // Struct()
}

#[derive(Debug, Clone, Default)]
pub enum Expr {
    Binding(BindingExpr), 
    /// (const name (fn [... params] (do ...body end)))
    Form(FormExpr),
    List(ExprList),
    Atom(ZValue),
    Newtype(NewtypeExpr),
}

#[derive(Debug, Clone)]
pub struct FuncDecl {

    pub name: ZIdent, 
    pub params: Option<AstList<ZIdent>>, 
    pub body: ExprList, 
}

impl FuncDecl {
    pub fn arity(&self) -> usize {
        if let Some(ps) = self.params.as_ref() {
            ps.len()
        } else {
            0
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
    Function(FuncDecl),
    Form(FormExpr),
    List(ExprList),
    Atom(ZValue),

    #[default]
    Nil,
}

#[derive(Debug, Clone)]
pub struct FuncExpr { 
    pub name: ZIdent, 
    pub args: Option<AstList<ZIdent>>, 
    pub body: ExprList, 
}

#[derive(Debug, Clone)]
pub struct Binding<Init> {
    pub name: ZIdent,
    pub rhs: Init,
}

pub type BindConst = Binding<Rc<Expr>>;
pub type BindLet = Binding<Rc<Expr>>;
pub type BindVar = Binding<Option<Rc<Expr>>>;
pub type BindFunc = Binding<Rc<FuncExpr>>;
pub type VarAssign = Binding<Rc<Expr>>;


// NOTE: Const is strictly a compile-time binding. Let cant
// be reassigned, but can be shadowed. Var can be reassigned, and not shadowed.
// Func variant has same rules as const, but initializer is specialized for Function Forms.
// ----------------------------------------------------------------------------------------
// ex: Self::Func -> const {name} = func(a: Int, b: Int): Int -> a + b end
#[derive(Debug, Clone)]
pub enum BindingExpr {
    
    Const(BindConst),
    Let(BindLet),
    Var(BindVar),
    Assign(VarAssign)
}

impl Expr {

    pub fn func_form(name: ZIdent, args: Option<AstList<ZIdent>>, body: ExprList) -> Self {
        let ff = FuncDecl{ name, params: args, body }; 
        let nt = NewtypeExpr::Function(ff);
        Self::Newtype(nt)

    }

    pub const fn is_block(&self) -> bool {
        matches!(self, Self::List(ExprList::Block(_)))
    } 



    pub fn func_form(name: ZIdent, args: Option<AstList<ZIdent>>, body: ExprList) -> Self {
        let ff = FuncDecl{ name, params: args, body }; 

        Self::Function(ff)
    }

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
        matches!(self, Expr::Form(FormExpr::When { .. }))
    }

    #[inline]
    pub fn try_list(&self) -> Option<ExprList> {
        match self {
            Expr::List(el) => Some(el.clone()),
            _ => None,
        }
    }

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
            Expr::Function(func_decl) => todo!(),
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
            Expr::List(le) => {
                match le {
                    ExprList::Block(_) => "List::Block", 
                    ExprList::Tuple(_) => "List::Tuple", 
                    ExprList::Nil => "List::Nil"
                }
            } 
            Expr::Atom(_) => "Atom",
            Expr::Nil => "Nil",
            Expr::Form(_) => "Call",
<<<<<<< HEAD:zeal.bak/src/ast/expr.rs

=======
>>>>>>> d77b955bc66bb133db314dfbd1c6e19ca0365611:zeal/src/ast/expr.rs
            Expr::Function(_) => "Function", 
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

    // #[inline]
    // pub fn binding(ty: VarType, head: ZIdent, init: Option<Expr>) -> Self {
    //     Self::Binding {
    //         ty,
    //         name: ZValue::Ident(head),
    //         init: Rc::from(init.unwrap_or_default()),
    //     }
    // }

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
        match ident.string() {
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

    // #[inline]
    // pub fn var_definition(name: ZIdent, init: Option<Expr>) -> Self {
    //     Self::binding(VarType::Var, name, init)
    // }
    //
    // #[inline]
    // pub fn let_definition(name: ZIdent, init: Option<Expr>) -> Self {
    //     Self::binding(VarType::Let, name, init)
    // }
    //
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
    use std::{fmt::Debug};
<<<<<<< HEAD:zeal.bak/src/ast/expr.rs

    use log::debug;
=======
>>>>>>> d77b955bc66bb133db314dfbd1c6e19ca0365611:zeal/src/ast/expr.rs

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
                    let mut cond_str = state.writeln("(when"); // info.fmtln("(cond");

                    state.inc_indent();

                    for cond in conds.iter() {
                        match cond.as_tup() {
                            (Expr::Nil, block) => {
                                let block_s = value_block(block, state);
                                let s = state.fmtln(&format!("else => {block_s}"));
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
                    cond_str.push_str(&state.fmtln("end)"));

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
                FormExpr::Print(ex) => format!("(print {})", expr(ex, state)),
                FormExpr::Println(ex) => {
                    format!("(println {})", expr(ex, state))
                }
                FormExpr::BinaryOperator { op, left, right } => {
                    let left = expr(left, state);
                    let right = expr(right, state);
                    format!("({op} {left} {right})")
                }
                // FormExpr::Assign { binding, rhs } => {
                    // format!("(#= {binding} {})", expr(rhs, state))
                // }
                FormExpr::Call { callee: lhs, params } => {
                    let head = expr(lhs.as_ref(), state);
                    let mut ps = String::new();
                    let params_str= if let Some(params) = params {
                        let res = params.iter().fold(&mut ps , |acc, e| { 
                            let e = expr(e, state); 
                            acc.push_str(&e); acc.push(' '); 
                            acc 
                        });
                        res.trim_end().to_string()

                    } else {
                        String::new()
                    };

                    state.writeln(&format!("(call {head} {params_str})"))

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
                FormExpr::Return { expr } => { 
                    let mut s = String::from("(return ");
                    let es = expr.as_ref().map(|e| { self::expr(e.as_ref(), state) }).unwrap_or_default(); 
                    s.push_str(&es);
                    s.push(')');
                    
                    state.writeln(&s)
                }
                FormExpr::Assign { binding, rhs } => todo!(), 
            },
            Expr::List(li) => exprlist(li, state),
            Expr::Atom(at) => at.to_string(),
            Expr::Binding(be) => {
                match be {
                     
                    // TODO: Cleanup these dups
                    BindingExpr::Const(be) => {
                        let name = &be.name;
                        let rhs = be.rhs.as_ref();
                        let rhs = expr(rhs, state);
                        let s = format!("(const {name} = {rhs})");
                        state.writeln(&s)
       
                    }
                    BindingExpr::Let(be) => {
                        let name = &be.name;
                        let rhs = be.rhs.as_ref();
                        let rhs = expr(rhs, state);
                        let s = format!("(let {name} = {rhs})");
                        state.writeln(&s)
 
                    }
                    BindingExpr::Assign(be) => {
                        let name = &be.name;
                        let rhs = be.rhs.as_ref();
                        let rhs = expr(rhs, state);
                        let s = format!("({name} = {rhs})");
                        state.writeln(&s)

                    } 

                    BindingExpr::Var(be) => {
                        let name = &be.name;
                        if let Some(rhs) = be.rhs.as_ref() {
                            let rhs = expr(rhs, state);
                            let s = format!("(var {name} = {rhs})");
                            state.writeln(&s)

                        } else {

                            let s = format!("(var {name} = nil)");
                            state.writeln(&s)
                        }

 
                    } 
                } 
                // let init = init.as_ref() {
                //     i.clone()
                // } else {
                //     Expr::Nil
                // };

                // let init_s = expr(init, state);
                // format!("({ty} {name}, {init_s})")

            }
            Expr::Nil => String::from("nil"),
<<<<<<< HEAD:zeal.bak/src/ast/expr.rs
            Expr::Newtype(NewtypeExpr::Function(FuncDecl { name, params: args, body })) => {
                
                    let params = { 
                        if let Some(args) = args.as_ref() {
                            let mut ps = String::new();
                            args.iter().fold(&mut ps , |acc, ident| { 
                                acc.push_str(ident.string()); 
                                acc.push(' '); 
                                acc 
                            });
                            ps

                        } else {
                            String::new()
                        }
                    };
                        
                let params = params.trim_end();
                let body = exprlist(body, state);
                state.writeln(&format!("(fn {name} [{params}] {body})"))


            }
        _ => todo!()
=======
            Expr::Function(func_decl) => {
                todo!()
            }, 
>>>>>>> d77b955bc66bb133db314dfbd1c6e19ca0365611:zeal/src/ast/expr.rs
        }

    }
}
impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut state = fmt::State { indent: 0, line: 1 };
        let s = fmt::expr(self, &mut state);

        write!(f, "{s}")
    }
}
