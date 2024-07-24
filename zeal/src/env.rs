use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    rc::Rc,
};

use anyhow::bail;

use crate::{ast::AstWalkError, core_types::ZValue, lex::Tok};

#[derive(Debug, Clone, Default)]
pub struct Scope {
    values: HashMap<String, ZValue>,
}

#[derive(Debug, Clone)]
pub struct Env {
    scope_stack: Vec<Scope>,
}

impl Env {
    /// Creates Environment with top level global scope
    pub fn new() -> Self {
        Self {
            scope_stack: vec![Scope::default()],
        }
    }

    #[inline]
    pub fn top(&self) -> &Scope {
        &self.scope_stack[0]
    }

    #[inline]
    pub fn top_mut(&mut self) -> &mut Scope {
        &mut self.scope_stack[0]
    }

    #[inline]
    pub fn bottom(&self) -> &Scope {
        let length = self.scope_stack.len();
        &self.scope_stack[length - 1]
    }

    #[inline]
    pub fn bottom_mut(&mut self) -> &mut Scope {
        let len = self.scope_stack.len();
        &mut self.scope_stack[len - 1]
    }

    pub fn pop_scope(&mut self) -> Option<Scope> {
        self.scope_stack.pop()
    }

    pub fn push_scope(&mut self, scope: Scope) {
        self.scope_stack.push(scope);
    }

    /// Defines variable at bottom level (inner-most) scope
    pub fn define(&mut self, name: &str, value: &ZValue) {
        self.bottom_mut()
            .values
            .insert(name.to_owned(), value.to_owned());
    }

    pub fn assign(&mut self, name: &Tok, value: &ZValue) -> anyhow::Result<()> {
        if let Some(scope) = self.find_scope_mut(name) {
            scope.values.insert(name.lexeme.clone(), value.clone());
            Ok(())
        } else {
            bail!(
                "{}",
                AstWalkError::RuntimeError {
                    token: name.tok.clone(),
                    message: format!("Undefined let binding '{}'.", &name.lexeme)
                }
            )
        }
    }

    pub fn get(&self, name: &Tok) -> anyhow::Result<ZValue> {
        if let Some(scope) = self.find_scope(name) {
            Ok(scope.values[&name.lexeme].clone())
        } else {
            bail!(
                "{}",
                AstWalkError::RuntimeError {
                    token: name.tok.clone(),
                    message: format!("Undefined variable: {}", &name.lexeme)
                }
            )
        }
    }

    fn find_scope(&self, name: &Tok) -> Option<&Scope> {
        self.scope_stack
            .iter()
            .rev()
            .find(|s| s.values.contains_key(&name.lexeme))
    }

    fn find_scope_mut(&mut self, name: &Tok) -> Option<&mut Scope> {
        self.scope_stack
            .iter_mut()
            .rev()
            .find(|s| s.values.contains_key(&name.lexeme))
    }
}
