use std::rc::Rc;

use anyhow::bail;
use phf::phf_map;

use crate::{
    ast::{lex::TokType, AstError, Expr, LexTok, Tok},
    core_types::{
        idents,
        num::{ZBool, ZFloat64},
        str::{ZIdent, ZString},
        val::ZValue,
    },
};

use super::{CallExpr, ExprList, VarType};

// pub static KEYWORDS: phf::Map<&'static str, LexTok> = phf_map! {
//     "struct" => LexTok::Struct,
//     "trait" => LexTok::Trait,
//     "impl" => LexTok::Impl,
//     "if" => LexTok::If,
//     "else" => LexTok::Else,
//     "true" => LexTok::Bool(true),
//     "false" => LexTok::Bool(false),
//     "fn" => LexTok::Fn,
//     "nil" => LexTok::Nil,
//     "and" => LexTok::And,
//     "or" => LexTok::Or,
//     "return" => LexTok::Return,
//     "super" => LexTok::Super,
//     "self" => LexTok::ThisSelf,
//     "let" => LexTok::Let,
//     "const" => LexTok::Const,
//     "loop" => LexTok::Loop,
//     "for" => LexTok::For,
//     "while" => LexTok::While,
//     "break" => LexTok::Break,
//     "match" => LexTok::Match,
//     "continue" => LexTok::Continue,
//     "println" => LexTok::Println,
//     "do" => LexTok::Do,
//     "end" => LexTok::End,
// };

#[derive(Debug, Clone)]
pub struct Parser {
    i: usize,
    tokens: Vec<Tok>,
}

impl Parser {
    pub fn parse_ast(tokens: &[Tok]) -> anyhow::Result<Expr> {
        let mut p = Self {
            i: 0,
            tokens: tokens.to_vec(),
        };
        let mut exprs = Vec::new();
        'parse: while !p.is_eof() {
            let ex = p.declaration()?;
            exprs.push(ex);
            while p.peek().ty == TokType::NewLine {
                if p.i == p.tokens.len() - 1 {
                    break 'parse;
                }
                p.advance(1);
            }
        }
        let exprs = Expr::tuple(exprs);
        Ok(exprs)
    }

    /// advance cursor to the next expression
    fn synchronize(&mut self) {
        self.advance(1);
        while !self.is_eof() {
            if let TokType::NewLine | TokType::Semicolon = self.prev().ty {
                break;
            }
            match self.peek().ty {
                TokType::Struct
                | TokType::Fn
                | TokType::Let
                | TokType::For
                | TokType::If
                | TokType::While
                | TokType::Println
                | TokType::Return => break,
                _ => self.advance(1),
            }
        }
    }

    // fn is_expr_end(&self) -> bool {
    //     match
    // }

    // Option as parsing an invalid declaration just results in that declaration getting ignored.
    // we should probably do some logging or error reporting at a higher level so invalid
    // declarations can be known about and arent completely silently ignored.
    fn declaration(&mut self) -> anyhow::Result<Expr> {
        let ty = match self.peek().ty {
            TokType::Let => Some(VarType::Let),
            TokType::Var => Some(VarType::Var),
            TokType::Const => Some(VarType::Const),
            _ => None,
        };
        if let Some(t) = ty {
            self.advance(1);
            self.let_expr(t)
        } else {
            self.statement_expr()
        }
    }
    //
    // fn let_statement(&mut self) -> anyhow::Result<Stmt> {
    //     if let TokType::Ident = self.peek().ty {
    //         let name = self.peek().clone();
    //         self.advance(1);
    //         let initializer = if let TokType::Eq = self.peek().ty {
    //             self.advance(1);
    //             Some(self.expression()?)
    //         } else {
    //             None
    //         };
    //
    //         if let TokType::Semicolon = self.peek().ty {
    //             let name = name.tok.clone();
    //             self.advance(1);
    //             Ok(Stmt::Let { name, initializer })
    //         } else {
    //             bail!(
    //                 "{}",
    //                 AstWalkError::ParseError {
    //                     token: self.peek().tok.clone(),
    //                     message: "Expected ';' after let statement".into()
    //                 }
    //             )
    //         }
    //     } else {
    //         bail!(
    //             "{}",
    //             AstWalkError::ParseError {
    //                 token: self.peek().tok.clone(),
    //                 message: "Expected variable name".into()
    //             }
    //         )
    //     }
    // }
    //
    //

    fn let_expr(&mut self, var_ty: VarType) -> anyhow::Result<Expr> {
        if let TokType::Ident = self.peek().ty {
            let name = self.peek().clone();
            self.advance(1);
            let initializer = if let TokType::Eq = self.peek().ty {
                self.advance(1);
                Some(self.statement_expr()?) // expression()?)
            } else {
                None
            };

            if let TokType::Semicolon | TokType::NewLine = self.peek().ty {
                let name = ZIdent::from(name);
                self.advance(1);
                let init = initializer.clone();
                let e = match var_ty {
                    VarType::Var => Expr::var_definition(name, init),
                    VarType::Let => Expr::let_definition(name, init),
                    VarType::Const => todo!(),
                };
                Ok(e)
            } else {
                bail!(
                    "{}",
                    AstError::ParseError {
                        expr: initializer.unwrap_or_else(|| Expr::Nil),
                        token: self.peek().tok.clone(),
                        message: "Expected ';' or newline after let statement".into()
                    }
                );
            }
        } else {
            bail!(
                "{}",
                AstError::ParseError {
                    expr: Expr::Nil,
                    token: self.peek().tok.clone(),
                    message: "Expected variable name".into()
                }
            )
        }
    }

    fn statement_expr(&mut self) -> anyhow::Result<Expr> {
        match self.peek().ty {
            TokType::Do | TokType::Begin => {
                self.advance(1);
                self.eat_newlines();
                let bs = self.block()?;
                Ok(bs)
                // Ok(Expr::Block(self.block()?))
            }
            // TODO: I dont like this. Make these Print* keywords into functions when
            // I get around to implementing them.
            TokType::Print => {
                self.advance(1);
                self.print_expr(PrintType::Fmt)
            }

            TokType::Println => {
                self.advance(1);
                self.print_expr(PrintType::Newline)
            }
            _ => self.statement(),
        }
    }

    fn block(&mut self) -> anyhow::Result<Expr> {
        let mut stmt_exprs = Vec::new();
        while self.peek().ty != TokType::End && !self.is_eof() {
            let st = self.statement_expr()?;

            stmt_exprs.push(st);
        }
        if let TokType::End = self.peek().ty {
            self.advance(1);
            // let se = stmt_exprs.into_boxed_slice().into();
            let bl = Expr::block(stmt_exprs);
            Ok(bl)
        } else {
            bail!(
                "{}",
                AstError::ParseError {
                    expr: Expr::block(stmt_exprs),
                    token: self.peek().tok.clone(),
                    message: "Expect 'end' after block.".into()
                }
            )
        }
    }

    fn eat_newlines(&mut self) {
        while self.peek().ty == TokType::NewLine {
            self.advance(1);
        }
    }
    fn print_expr(&mut self, ty: PrintType) -> anyhow::Result<Expr> {
        // let expr = self.statement()?;
        let expr = self.expression()?;
        if let TokType::Semicolon | TokType::NewLine = self.peek().ty {
            self.advance(1);
            let head = match ty {
                PrintType::Fmt => ZIdent::new(idents::PRINT),
                PrintType::Newline => ZIdent::new(idents::PRINTLN),
            }
            .into();

            let cl = CallExpr {
                head,
                params: Rc::from([expr]),
            };
            let e = Expr::Call(cl);
            Ok(e)
        } else {
            bail!(
                "{}",
                AstError::ParseError {
                    token: self.peek().tok.clone(),
                    message: "Expected newline or ';' at end of print* expression.".into(),
                    expr: expr
                }
            )
        }
    }

    fn statement(&mut self) -> anyhow::Result<Expr> {
        let expr = self.expression()?;
        if let TokType::Semicolon | TokType::NewLine = self.peek().ty {
            self.advance(1);
            Ok(expr)
        } else {
            bail!(
                "{}",
                AstError::ParseError {
                    expr: expr,
                    token: self.peek().tok.clone(),
                    message: "Expected newline or ';' after expression".into()
                }
            )
        }
    }

    fn assignment(&mut self) -> anyhow::Result<Expr> {
        let expr = self.equality()?;

        if let TokType::Eq = self.peek().ty {
            self.advance(1);
            let equals = self.prev().clone();
            let value = self.assignment()?;
            if let Some(sym) = expr.inner_ident() {
                Ok(Expr::assignment(sym.clone(), value.clone()))
            } else {
                bail!(
                    "{}",
                    AstError::ParseError {
                        expr: expr,
                        token: equals.tok.clone(),
                        message: "Invalid assignment. only identifiers are allowed on left hand side of assignmnent exprs".into()
                    }
                )
            }
        } else {
            Ok(expr)
        }
    }

    fn expression(&mut self) -> anyhow::Result<Expr> {
        self.assignment()
    }
    fn term(&mut self) -> anyhow::Result<Expr> {
        // self.expand_binary_expr(ExprRule::Factor, &[TokType::Minus, TokType::Plus])
        let mut expr = self.factor()?;
        loop {
            match self.peek().ty {
                TokType::Minus | TokType::Plus => {
                    self.advance(1);
                    let operator = self.prev().clone().into_ident();
                    let right = self.factor()?;

                    expr = Expr::binary_op(&expr, &operator, &right);
                    // expr = Expr::Binary {
                    //     left: Box::new(expr),
                    //     operator,
                    //     right: Box::new(right),
                    // }
                }
                _ => break,
            }
        }
        Ok(expr)
    }
    fn comparison(&mut self) -> anyhow::Result<Expr> {
        let mut expr = self.term()?;
        loop {
            match self.peek().ty {
                TokType::Gt
                | TokType::Ge
                | TokType::Lt
                | TokType::Le
                | TokType::EqEq
                | TokType::BangEq => {
                    self.advance(1);
                    let operator = self.prev().clone().into_ident();
                    let right = self.term()?;
                    expr = Expr::binary_op(&expr, &operator, &right);
                    // expr = Expr::Binary {
                    //     left: Box::new(expr),
                    //     operator,
                    //     right: Box::new(right),
                    // }
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> anyhow::Result<Expr> {
        let mut expr = self.unary()?;
        loop {
            match self.peek().ty {
                TokType::ForwardSlash | TokType::Star => {
                    self.advance(1);
                    let operator = self.prev().clone().into_ident();
                    let right = self.unary()?;
                    expr = Expr::binary_op(&expr, &operator, &right);
                    // expr = Expr::Binary {
                    //     left: Box::new(expr),
                    //     operator,
                    //     right: Box::new(right),
                    // }
                }
                _ => break,
            }
        }

        Ok(expr)
    }
    fn unary(&mut self) -> anyhow::Result<Expr> {
        match self.peek().ty {
            TokType::Bang | TokType::Minus => {
                self.advance(1);
                let operator = self.prev().clone().into_ident();
                let right = self.unary()?;
                let expr = Expr::unary_op(&operator, &right);
                Ok(expr)
                // Ok(Expr::Unary {
                //     operator,
                //     right: Box::new(right),
                // })
            }
            _ => self.primary(),
        }
    }

    fn primary(&mut self) -> anyhow::Result<Expr> {
        match self.peek().tok {
            LexTok::Bool(false) => {
                self.advance(1);
                Ok(Expr::Atom(ZValue::Bool(ZBool::from(false))))
            }
            LexTok::Bool(true) => {
                self.advance(1);
                Ok(Expr::Atom(ZValue::Bool(ZBool::from(true))))
            }
            LexTok::Nil => {
                self.advance(1);
                Ok(Expr::Atom(ZValue::Nil))
            }
            LexTok::Number(ref n) => {
                let n = *n;
                self.advance(1);
                Ok(Expr::Atom(ZValue::Number(ZFloat64::from(n))))
            }
            LexTok::String(ref s) => {
                let s = s.clone();
                let s = s.trim_matches('"');
                self.advance(1);
                Ok(Expr::Atom(ZValue::Str(ZString::from(s))))
            }
            LexTok::OpenParen => {
                self.advance(1);
                let expr = self.expression()?;
                if self.peek().ty == TokType::CloseParen {
                    self.advance(1);
                    Ok(expr)
                    // Ok(Expr::Grouping(Box::new(expr)))
                } else {
                    Err(anyhow::anyhow!(
                        "Expected matching ending right parenthesis in expression"
                    ))
                }
            }
            LexTok::Ident => {
                let name = self.peek().clone();
                self.advance(1);
                Ok(Expr::Atom(ZValue::Ident(name.into_ident())))
            }
            _ => Err(anyhow::anyhow!(
                "Expected primary or group expression, found: {:?}",
                self.peek()
            )),
        }
    }

    fn equality(&mut self) -> anyhow::Result<Expr> {
        let mut expr = self.comparison()?;
        loop {
            match self.peek().ty {
                TokType::BangEq | TokType::EqEq => {
                    self.advance(1);
                    let operator = self.prev().clone().into_ident();
                    let right = self.comparison()?;
                    expr = Expr::binary_op(&expr, &operator, &right);
                    // expr = Expr::Binary {
                    //     left: Box::new(expr),
                    //     operator,
                    //     right: Box::new(right),
                    // }
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn is_eof(&self) -> bool {
        self.peek().ty == TokType::Eof
    }

    fn advance(&mut self, n: usize) {
        assert!(!self.is_eof(), "Tried to advance cursor at EOF token");
        assert!(
            self.i + n < self.tokens.len(),
            "advancing cursor past end of tokens. len: {}, i: {}",
            self.tokens.len(),
            self.i + n
        );
        self.i += n;
    }

    fn peek(&self) -> &Tok {
        &self.tokens[self.i]
    }

    fn prev(&self) -> &Tok {
        &self.tokens[self.i - 1]
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PrintType {
    Fmt,
    Newline,
}
