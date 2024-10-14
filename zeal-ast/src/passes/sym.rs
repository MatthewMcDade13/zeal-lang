use std::ops::{Deref, DerefMut};

use crate::expr::Expr;

pub struct SymbolTable {}

#[derive(Debug, Clone)]
pub struct Meta {}

#[derive(Debug, Clone)]
pub struct AstNode {
    pub node: Expr,
    pub meta: Meta,
}

impl AsRef<Expr> for AstNode {
    fn as_ref(&self) -> &Expr {
        &self.node
    }
}

impl Deref for AstNode {
    type Target = Expr;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl DerefMut for AstNode {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.node
    }
}
