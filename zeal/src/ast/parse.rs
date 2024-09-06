use std::rc::Rc;

use anyhow::{anyhow, bail, ensure};

use crate::{
    ast::{
        lex::{LineInfo, TokType},
        Expr, ExprList, LexTok, Tok,
    },
    core_types::{
        idents,
        num::{ZBool, ZFloat64},
        str::{ZIdent, ZString},
        val::ZValue,
    },
    err::{
        self,
        parse::{ExprInfo, ParseErrInfo, ParseError},
    },
};

use super::{
    expr::{CondForm, FormExpr, LoopExpr},
    VarType,
};

macro_rules! blocklike {
    ($self: ident, $start_pat: pat, $end_pat: pat) => {{
        if let $start_pat = $self.peek().ty {
            $self.adv(1)?;
            let mut stmt_exprs = Vec::new();
            loop {
                if $self.is_eof() {
                    break Ok(ExprList::block(stmt_exprs));
                }

                if let $end_pat = $self.peek().ty {
                    break Ok(ExprList::block(stmt_exprs));
                }

                let st = $self.expression_stmt()?;

                stmt_exprs.push(st);
            }
        } else {
            anyhow::Result::Err(anyhow!(
                "{}",
                ParseError::ExpectedBlock(ParseErrInfo::from($self, None))
            ))
        }
    }};
}

impl ParseErrInfo {
    pub fn from(p: &Parser, expr: Option<&Expr>) -> Self {
        let expr = expr.map(|e| err::parse::ExprInfo {
            ty: e.type_str().to_owned(),
            string: e.to_string(),
        });
        Self {
            expr,
            prev: p.prev().clone(),
            curr: p.peek().clone(),
            next: None,
        }
    }
}
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
        while !p.is_eof() {
            match p.expression_stmt() {
                Ok(ex) => exprs.push(ex),
                Err(e) => {
                    if let Some(ParseError::Eof) = e.downcast_ref::<ParseError>() {
                        break;
                    } else {
                        let LineInfo { line, col } = p.peek().info;
                        println!(
                            // TODO: Make this toggleable, idk like to have the option to
                            // abort parsing or just skip if we cant parse an expression.
                            "Error parsing expression at (L:{},C:{},I:{}) Skipping. =>  {}",
                            line, col, p.i, e
                        );
                        continue;
                    }
                }
            }
        }
        let exprs = Expr::tuple(exprs);
        Ok(exprs)
    }

    fn loop_body(&mut self) -> anyhow::Result<ExprList> {
        if let TokType::Do | TokType::Begin = self.peek().ty {
            self.adv(1)?;
            let mut stmt_exprs = Vec::new();
            loop {
                if self.is_eof() {
                    break Ok(ExprList::Block(stmt_exprs.into()));
                }

                if let TokType::End = self.peek().ty {
                    break Ok(ExprList::Block(stmt_exprs.into()));
                }

                let st = self.expression_stmt()?;

                stmt_exprs.push(st);
            }
        } else {
            anyhow::Result::Err(anyhow!(
                "{}",
                ParseError::ExpectedBlock(ParseErrInfo::from(self, None))
            ))
        }
    }

    /// top level statement in Zeal. Zeal programs are (for now, until i implement modules) the
    /// made up of expression statement 'building blocks'.
    /// i.e.: program ::= expression_stmt* EOF
    fn expression_stmt(&mut self) -> anyhow::Result<Expr> {
        match self.peek().ty {
            TokType::Let => {
                self.adv(1)?;
                self.bind_expr(VarType::Let)
            }
            TokType::Var => {
                self.adv(1)?;
                self.bind_expr(VarType::Var)
            }
            TokType::Const => {
                self.adv(1)?;
                self.bind_expr(VarType::Const)
            }
            TokType::Print => {
                self.adv(1)?;
                self.print_expr(PrintType::Fmt)
            }
            TokType::Println => {
                self.adv(1)?;
                self.print_expr(PrintType::Newline)
            }
            TokType::If => {
                self.adv(1)?;
                self.if_expr()
            }
            TokType::Loop => {
                self.adv(1)?;
                self.loop_expr()
            }
            TokType::While => {
                self.adv(1)?;
                self.while_expr()
            }
            TokType::Each => {
                todo!()
            }
            TokType::For => {
                todo!()
            }

            TokType::Eof => bail!("{}", ParseError::Eof),
            _ => self.block_expr(),
        }
    }

    fn loop_expr(&mut self) -> anyhow::Result<Expr> {
        let loop_body = blocklike!(self, TokType::Do | TokType::Begin, TokType::End)?;
        self.adv(1)?;
        let expr = Expr::loop_form(loop_body, LoopExpr::Loop);
        Ok(expr)
    }

    fn while_expr(&mut self) -> anyhow::Result<Expr> {
        let cond = self.expression()?;
        let cond = Rc::new(cond);

        let loop_body = blocklike!(self, TokType::Do | TokType::Begin, TokType::End)?;
        self.adv(1)?;
        let expr = Expr::loop_form(loop_body, LoopExpr::While { cond });
        Ok(expr)
    }

    fn if_expr(&mut self) -> anyhow::Result<Expr> {
        // let cond = self.expression()?;
        // let cond = Rc::new(cond);
        //
        // let if_body = blocklike!(
        //     self,
        //     TokType::Then | TokType::Do,
        //     TokType::End | TokType::Else | TokType::ElseIf
        // )?;

        let mut branches = Vec::<CondForm>::new();
        while !self.is_eof() {
            let cond = self.expression()?;

            let body = blocklike!(
                self,
                TokType::Then | TokType::Do,
                TokType::End | TokType::Else | TokType::ElseIf
            )?;
            branches.push(CondForm::Branch(cond, body));

            match self.peek().ty {
                TokType::End => {
                    self.adv(1)?;
                    branches.push(CondForm::End);
                    break;
                }
                TokType::Else => {
                    let else_body = blocklike!(self, TokType::Else, TokType::End)?;
                    self.adv(1)?;
                    branches.push(CondForm::Else(else_body));
                    branches.push(CondForm::End);

                    break;
                }
                TokType::ElseIf => {
                    self.adv(1)?;
                    continue;
                }
                _ => unreachable!(),
            }
        }
        ensure!(
            branches.last().unwrap().is_end(),
            "Last branch of cond (if) form must be '_' (else)"
        );
        let form = Expr::cond_form(branches);
        Ok(form)
    }

    fn bind_expr(&mut self, var_ty: VarType) -> anyhow::Result<Expr> {
        if let TokType::Ident = self.peek().ty {
            let name = self.peek().clone();
            self.adv(1)?;
            let initializer = if let TokType::Eq = self.peek().ty {
                self.adv(1)?;
                Some(self.expression()?) // expression()?)
            } else {
                None
            };
            let name = ZIdent::from(name);
            let init = initializer.clone();
            let e = match var_ty {
                VarType::Var => Expr::var_definition(name, init),
                VarType::Let => Expr::let_definition(name, init),
                VarType::Const => todo!(),
            };
            Ok(e)
        } else {
            let info = ParseErrInfo::from(self, None);
            bail!(
                "{}",
                ParseError::UnexpectedBindName {
                    got: self.peek().clone().to_string(),
                    info
                }
            )
        }
    }

    fn block_expr(&mut self) -> anyhow::Result<Expr> {
        if let TokType::Do | TokType::Begin = self.peek().ty {
            let block = blocklike!(self, TokType::Begin | TokType::Do, TokType::End);
            block.map(|b| b.into_expr())
        } else {
            self.expression()
        }
    }

    fn print_expr(&mut self, ty: PrintType) -> anyhow::Result<Expr> {
        let expr = self.expression()?;
        let expr = Rc::new(expr);

        let form = match ty {
            PrintType::Fmt => FormExpr::Print(expr),
            PrintType::Newline => FormExpr::Println(expr),
        };
        let fe = Expr::Form(form);
        Ok(fe)
    }

    fn is_terminal(&self) -> bool {
        if let TokType::Semicolon | TokType::NewLine = self.peek().ty {
            true
        } else {
            false
        }
    }

    fn eat_terminals(&mut self) -> anyhow::Result<()> {
        while self.is_terminal() && !self.is_eof() {
            self.adv(1)?;
        }
        Ok(())
    }

    fn logical_bitwise(&mut self) -> anyhow::Result<Expr> {
        let mut expr = self.comparison()?;
        loop {
            match self.peek().ty {
                TokType::Or | TokType::And => {
                    self.adv(1)?;
                    let operator = self.prev().clone().into_ident();
                    let right = self.comparison()?;

                    expr = Expr::binary_op(expr, operator, right);
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    #[inline]
    fn expression(&mut self) -> anyhow::Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> anyhow::Result<Expr> {
        let expr = self.logical_bitwise()?;

        if let TokType::Eq = self.peek().ty {
            self.adv(1)?;
            let value = self.assignment()?;
            if let Some(sym) = expr.inner_ident() {
                Ok(Expr::assignment(sym.clone(), value.clone()))
            } else {
                bail!("{}", ParseError::InvalidAssignment)
            }
        } else {
            Ok(expr)
        }
    }

    fn term(&mut self) -> anyhow::Result<Expr> {
        let mut expr = self.factor()?;
        loop {
            match self.peek().ty {
                TokType::Minus | TokType::Plus => {
                    self.adv(1)?;
                    let operator = self.prev().clone().into_ident();
                    let right = self.factor()?;

                    expr = Expr::binary_op(expr, operator, right);
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
                    self.adv(1)?;
                    let operator = self.prev().clone().into_ident();
                    let right = self.term()?;
                    expr = Expr::binary_op(expr, operator, right);
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
                    self.adv(1)?;
                    let operator = self.prev().clone().into_ident();
                    let right = self.unary()?;
                    expr = Expr::binary_op(expr, operator, right);
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> anyhow::Result<Expr> {
        match self.peek().ty {
            TokType::Bang | TokType::Minus => {
                self.adv(1)?;
                let operator = self.prev().clone().into_ident();
                let right = self.unary()?;
                let expr = Expr::unary_op(operator, right);
                Ok(expr)
            }
            _ => self.primary(),
        }
    }

    fn primary(&mut self) -> anyhow::Result<Expr> {
        match self.peek().tok {
            LexTok::Bool(false) => {
                self.adv(1)?;
                Ok(Expr::Atom(ZValue::Bool(ZBool::from(false))))
            }
            LexTok::Bool(true) => {
                self.adv(1)?;
                Ok(Expr::Atom(ZValue::Bool(ZBool::from(true))))
            }
            LexTok::Nil => {
                self.adv(1)?;
                Ok(Expr::Atom(ZValue::Nil))
            }
            LexTok::Number(ref n) => {
                let n = *n;
                self.adv(1)?;
                Ok(Expr::Atom(ZValue::Number(ZFloat64::from(n))))
            }
            LexTok::String(ref s) => {
                let s = s.clone();
                let s = s.trim_matches('"');
                self.adv(1)?;
                Ok(Expr::Atom(ZValue::Str(ZString::from(s))))
            }
            LexTok::OpenParen => {
                self.adv(1)?;
                let expr = self.expression()?;
                if self.peek().ty == TokType::CloseParen {
                    self.adv(1)?;
                    Ok(expr)
                } else {
                    Err(anyhow::anyhow!(
                        "Expected matching ending right parenthesis in expression"
                    ))
                }
            }
            LexTok::Ident => {
                let name = self.peek().clone();
                self.adv(1)?;
                Ok(Expr::Atom(ZValue::Ident(name.into_ident())))
            }
            _ => {
                self.adv(1)?;
                self.eat_terminals()?;
                // an error somewhere, retry.
                let expr = self.expression_stmt()?;

                Ok(expr)
            }
        }
    }

    fn equality(&mut self) -> anyhow::Result<Expr> {
        let mut expr = self.comparison()?;
        loop {
            match self.peek().ty {
                TokType::BangEq | TokType::EqEq => {
                    self.adv(1)?;
                    let operator = self.prev().clone().into_ident();
                    let right = self.comparison()?;
                    expr = Expr::binary_op(expr, operator, right);
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn is_eof(&self) -> bool {
        self.peek().ty == TokType::Eof
    }

    /// returns true if advance is successfull, false otherwise.
    fn adv(&mut self, n: usize) -> anyhow::Result<()> {
        let i = self.i + n;
        if self.is_eof() || i >= self.tokens.len() {
            bail!("{}", ParseError::Eof)
        } else {
            self.i += n;
            Ok(())
        }
    }

    #[inline]
    fn peek(&self) -> &Tok {
        &self.tokens[self.i]
    }

    #[inline]
    fn prev(&self) -> &Tok {
        &self.tokens[self.i - 1]
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PrintType {
    Fmt,
    Newline,
}
