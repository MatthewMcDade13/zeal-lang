use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use crate::{
    ast::{
        expr::{Expr, ExprList},
        Ast,
    },
    core_types::str::ZIdent,
};

use super::{
    types::{ProtoShape, Prototype, Type, TypeInfo, TypeName, TypedIdent},
    MemLocation, Meta, Visibility,
};

#[derive(Debug, Clone)]
pub struct SymTable {
    parent_scope: Option<Box<SymTable>>,
    table: HashMap<ZIdent, Meta>,
}
impl Deref for SymTable {
    type Target = HashMap<ZIdent, Meta>;

    fn deref(&self) -> &Self::Target {
        &self.table
    }
}

impl DerefMut for SymTable {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.table
    }
}

impl SymTable {
    pub fn lookup(&self, ident: &ZIdent) -> Option<&Meta> {
        self.table.get(ident)
    }

    pub fn add_untyped(&mut self, name: ZIdent) {
        let k = name.clone();
        self.table.insert(k, Meta::any(name));
    }

    /// Adds identifer to symbol table with given type.
    /// Use add_func to add function to symbol table
    /// DOES NOT check if symbol is already in table.
    pub fn add_var(&mut self, name: ZIdent, ty: Type) -> Option<usize> {
        // TODO: Use bloom filter to ensure we dont have an item in symbol table
        // before we try and do a lookup.
        let key = name.clone();
        let st = TypedIdent::Variable(TypeName { name, ty });

        let ti = TypeInfo {
            size_bytes: ty.min_size_bytes(),
            sym: st,
        };
        let i = self.table.len();
        let m = Meta {
            type_info: ti,
            name: key.clone(),
            ..Default::default()
        };

        self.table.insert(key, m);
        Some(i)
    }

    /// DOES NOT check if symbol is already in table
    pub fn add_func(&mut self, name: ZIdent, proto: Prototype) {
        let fname = name.clone();
        let v = Meta::function(fname, proto);

        self.table.insert(name, v);
    }

    pub fn lookup_func(&self, name: &ZIdent, arity: usize, reciever: Type) -> Option<&Prototype> {
        if let Some(meta) = self.table.get(name) {
            if let TypedIdent::Function(proto) = &meta.type_info.sym {
                Some(proto)
            } else {
                None
            }
        } else if let Some(parent) = self.parent_scope.as_ref() {
            parent.lookup_func(name, arity, reciever)
        } else {
            None
        }
    }
}

// pub trait FoldAst<Output> {
//     fn next_expr(&mut self, expression: &Expr) -> anyhow::Result<Output>;
//     fn next_exprlist(&mut self, expression_list: &ExprList) -> anyhow::Result<Output>;
// }
//
// // pub trait CompResult<T = Self> {
// //     fn pipe(self) -> T;
// // }
// //
//
// impl FoldAst<()> for Ast {
//     fn next_expr(&mut self, expression: &Expr) -> anyhow::Result<()> {
//         todo!()
//     }
//
//     fn next_exprlist(&mut self, expression_list: &ExprList) -> anyhow::Result<()> {
//         todo!()
//     }
// }
// impl FoldAst<String> for Ast {
//     fn next_expr(&mut self, expression: &Expr) -> anyhow::Result<String> {
//         todo!()
//     }
//
//     fn next_exprlist(&mut self, expression_list: &ExprList) -> anyhow::Result<String> {
//         todo!()
//     }
// }
