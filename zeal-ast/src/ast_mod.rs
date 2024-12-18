use std::{fmt::Display, path::PathBuf, rc::Rc};

use crate::{
    expr::{AstList, ExprStmt},
    lex::{Tok, TokBuffer},
    parse::Parser,
    AstStringify,
};

#[derive(Debug, Clone)]
pub struct AstModule {
    pub name: String,
    pub ast: AstList<ExprStmt>,
    pub path: PathBuf,
}

impl AstModule {
    pub fn from_toks(toks: &[Tok]) -> anyhow::Result<Self> {
        let ast = Parser::parse_ast(toks)?;
        // let symbols = Self::build_symbol_table(&tree);
        Ok(ast)
        // Ok(Self(ast))
    }

    pub fn from_file(path: &str) -> anyhow::Result<Self> {
        let buf = TokBuffer::read_file(path)?;
        let s = Self::from_toks(buf.slice())?;
        Ok(s)
    }

    pub fn without_meta(ast: AstList<ExprStmt>) -> Self {
        Self {
            name: String::new(),
            ast,
            path: PathBuf::new(),
        }
    }
}

impl std::str::FromStr for AstModule {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let buf = TokBuffer::read_string(s)?;
        let s = Self::from_toks(buf.slice())?;
        Ok(s)
    }
}

impl Display for AstModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = String::from("----- AST ----- \n");
        if let AstList::List(l) = &self.ast {
            for e in l.iter() {
                if let Ok(est) = AstStringify::expr_stmt_tostring(e) {
                    res.push_str(&format!("{est}\n"));
                } else {
                    return std::fmt::Result::Err(std::fmt::Error);
                }
            }
        }
        res.push_str("----- END AST -----\n");
        write!(f, "{}", res)
    }
}
