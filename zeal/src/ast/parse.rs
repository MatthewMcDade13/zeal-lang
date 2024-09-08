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
    expr::{FormExpr, LoopExpr, WhenForm},
    VarType,
};

macro_rules! loop_block {
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

/// Tries to match pattern $try_start_pat and then parses
/// section until given $end_pat pattern is peeked.
/// (Does not advance past tok matched by $end_pat)
macro_rules! try_block {
    ($self: ident, $try_start_pat: pat, $end_pat: pat) => {{
        if let $try_start_pat = $self.peek().ty {
            $self.adv(1)?;
            let mut stmt_exprs = Vec::new();
            loop {
                if $self.is_eof() {
                    break Some(ExprList::block(stmt_exprs));
                }

                if let $end_pat = $self.peek().ty {
                    break Some(ExprList::block(stmt_exprs));
                }

                let st = $self.expression_stmt()?;

                stmt_exprs.push(st);
            }
        } else {
            None
        }
    }};
}

/// Same as try_block, but returns Err if initial $start_pat pattern does
/// not match current self.peek().ty.
///
///
/// Tries to match pattern $try_start_pat and then parses
/// section until given $end_pat pattern is peeked.
/// (Does not advance past tok matched by $end_pat)
macro_rules! block {
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
            
            TokType::When => {
                self.adv(1)?;
                self.when_expr()
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
            // TODO: For now break statements do not take an optional 'return' expression (similar
            // to rust break). We need to implement later. for now im not too bothered lol.
            TokType::Break => {
                self.adv(1)?;
                let e = Expr::break_form(None);
                Ok(e)
            }

            TokType::Eof => bail!("{}", ParseError::Eof),
            _ => self.place_value_block(),
        }
    }

    fn loop_expr(&mut self) -> anyhow::Result<Expr> {
        let loop_body = block!(self, TokType::Do | TokType::Begin, TokType::End)?;
        self.adv(1)?;
        let expr = Expr::loop_form(loop_body, LoopExpr::Loop);
        Ok(expr)
    }

    fn while_expr(&mut self) -> anyhow::Result<Expr> {
        let cond = self.expression()?;
        let cond = Rc::new(cond);

        let loop_body = block!(self, TokType::Do | TokType::Begin, TokType::End)?;
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

        let mut branches = Vec::<WhenForm>::new();
        while !self.is_eof() {
            let cond = self.expression()?;

            let body = block!(
                self,
                TokType::Then | TokType::Do,
                TokType::End | TokType::Else | TokType::ElseIf
            )?;
            branches.push(WhenForm::Branch(cond, body.into_expr()));

            match self.peek().ty {
                TokType::End => {
                    self.adv(1)?;
                    branches.push(WhenForm::End);
                    break;
                }
                TokType::Else => {
                    let else_body = block!(self, TokType::Else, TokType::End)?;
                    self.adv(1)?;
                    branches.push(WhenForm::Else(else_body.into_expr()));
                    branches.push(WhenForm::End);

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
        let form = Expr::when_form(branches);
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
    fn when_expr(&mut self) -> anyhow::Result<Expr> {
        let mut branches: Vec<WhenForm> = Vec::new();
        while self.peek().ty != TokType::End && !self.is_eof() {
            if matches!(self.peek().ty, TokType::Else) {
              
                self.adv(1)?;
                ensure!(
                    matches!(self.peek().ty, TokType::FatArrow),
                    "Expected '=>' after 'else' in when expression"
                );
                self.adv(1)?;
                let then = self.place_value_block()?;
                branches.push(WhenForm::Else(then));
                branches.push(WhenForm::End);
                break;

            } else {
                let when_cond = self.expression()?;
                ensure!(
                    matches!(self.peek().ty, TokType::FatArrow),
                        "Parse Error: '=>' required in when expressions in between branch left hand conditions and right hand blocks/exprs. Ex: cond_expr => block end",
                    
                );
                self.adv(1)?;
                let then = self.place_value_block()?;
                if !then.is_block() {
                    // require trailing comma if then branch is anything but a block.
                    ensure!(self.peek().ty == TokType::Comma,"require trailing comma if then branch is anything but a block.");
                     self.adv(1)?;
                }
                branches.push(WhenForm::Branch(when_cond, then));
            }
        }
        self.adv(1)?;
        let w = Expr::when_form(branches);
        Ok(w)
    }

    /// Block expression that can be either:
    ///
    ///     place:
    ///         begin
    ///             println 50
    ///         end
    ///
    ///             vs.
    ///
    ///     value:
    ///     let x =  begin
    ///         50
    ///     end
    ///
    /// TODO: Implement tracking if expression is place or value.
    ///
    fn place_value_block(&mut self) -> anyhow::Result<Expr> {
        if let Some(bl) = try_block!(self, TokType::Begin | TokType::Do, TokType::End) {
            Ok(bl.into_expr())
        } else {
            self.expression()
        }
    }

    fn value_block(&mut self) -> anyhow::Result<Expr> {
        if let TokType::Do | TokType::Begin = self.peek().ty {
            let block = block!(self, TokType::Begin | TokType::Do, TokType::End);
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
