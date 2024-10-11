use anyhow::{anyhow, bail, ensure, Context};
use std::rc::Rc;

use crate::{
    err::{ParseErrInfo, ParseError},
    expr::{AstList, AstRune, BindType, Binding, Expr, ExprStmt, WhenForm},
    lex::{LexTok, LineInfo, Tok, TokType},
    Ast,
};

/// Tries to match pattern $try_start_pat and then parses
/// section until given $end_pat pattern is peeked.
/// (Does not advance past tok matched by $end_pat)
macro_rules! try_block {
    ($self: ident, $end_pat: pat) => {{
        let mut stmt_exprs = Vec::new();
        loop {
            if $self.is_eof() {
                break Some(stmt_exprs);
            }

            if let $end_pat = $self.peek().ty {
                break Some(stmt_exprs);
            }

            let st = $self.expression_stmt()?;

            stmt_exprs.push(st);
        }
    }};
    ($self: ident, $try_start_pat: pat, $end_pat: pat) => {{
        if let $try_start_pat = $self.peek().ty {
            $self.adv(1)?;
            try_block!($self, $end_pat)
            // let mut stmt_exprs = Vec::new();
            // loop {
            //     if $self.is_eof() {
            //         break Some(ExprList::block(stmt_exprs));
            //     }
            //
            //     if let $end_pat = $self.peek().ty {
            //         break Some(ExprList::block(stmt_exprs));
            //     }
            //
            //     let st = $self.expression_stmt()?;
            //
            //     stmt_exprs.push(st);
            // }
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
    ($self: ident, $end_pat: pat) => {{
        let mut stmt_exprs: Vec<ExprStmt> = Vec::new();
        loop {
            if $self.is_eof() {
                break Ok(stmt_exprs);
            }

            if let $end_pat = $self.peek().ty {
                break Ok(stmt_exprs);
            }

            let st = $self.expression_stmt()?;

            stmt_exprs.push(st);
        }
    }};
    ($self: ident, $start_pat: pat, $end_pat: pat) => {{
        if let $start_pat = $self.peek().ty {
            $self.adv(1)?;
            block!($self, $end_pat)
        } else {
            anyhow::Result::Err(anyhow!(
                "{}",
                ParseError::ExpectedBlock(ParseErrInfo {
                    expr: None,
                    prev: $self.peek_prev().clone(),
                    curr: $self.peek().clone(),
                    next: $self.peek_next().map(|t| t.clone()),
                })
            ))
        }
    }};
}

#[derive(Debug, Default, Clone, Copy)]
pub enum BlockType {
    /// ''->' to '',''
    WhenBranch,
    WhenElse,
    // 'do' to 'end'
    #[default]
    Do,
    /// 'begin' to 'end'
    Begin,
}

#[derive(Debug, Clone)]
pub struct Parser {
    pub i: usize,
    pub tokens: Vec<Tok>,
}

impl Parser {
    pub fn parse_ast(tokens: &[Tok]) -> anyhow::Result<Ast> {
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
        let tree = AstList::new(exprs);
        let ast = Ast { tree };
        Ok(ast)
    }

    fn loop_body(&mut self) -> anyhow::Result<AstList<ExprStmt>> {
        if let TokType::Do | TokType::Begin = self.peek().ty {
            self.adv(1)?;
            let mut stmt_exprs = Vec::new();
            loop {
                if self.is_eof() {
                    break Ok(AstList::new(stmt_exprs));
                }

                if let TokType::End = self.peek().ty {
                    break Ok(AstList::new(stmt_exprs));
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
    fn expression_stmt(&mut self) -> anyhow::Result<ExprStmt> {
        match self.peek().ty {
            TokType::Let => {
                self.adv(1)?;
                self.bind_expr(BindType::Let)
            }
            TokType::Var => {
                self.adv(1)?;
                self.bind_expr(BindType::Var)
            }
            TokType::Const => {
                self.adv(1)?;
                self.bind_expr(BindType::Const)
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
                let e = ExprStmt::break_stmt(Expr::Unit);
                Ok(e)
            }

            TokType::Eof => bail!("{}", ParseError::Eof),
            _ => self.place_value_block(BlockType::default()),
        }
    }

    fn func_decl(&mut self) -> anyhow::Result<ExprStmt> {
        if matches!(self.peek().ty, TokType::Ident) {
            let name = self.peek().clone().into_ast_rune();
            self.adv(1)?;
            if matches!(self.peek().ty, TokType::OpenParen) {
                self.adv(1)?;
                let params = self.parameter_list()?;

                // TODO: Add function return type parsing here
                let body = try_block!(self, TokType::End)
                    .context("Error parsing function body. Missing 'end'?")?;
                ensure!(
                    matches!(self.peek().ty, TokType::End),
                    "Expected 'end'. Got: {:?}",
                    self.peek_ty()
                );
                self.adv(1)?;
                let body = ExprStmt::block(body);
                let f = ExprStmt::func_decl(name, params, body);

                Ok(f)
            } else {
                bail!(
                    "Expeceted '(' after function name =>\n\t{}",
                    ParseError::UnexpectedToken(ParseErrInfo::from(self, None))
                );
            }
        } else {
            bail!(
                "Expected identifer after 'fn' =>\n\t{}",
                ParseError::UnexpectedToken(ParseErrInfo::from(self, None))
            )
        }
    }

    #[inline]
    fn peek_ty(&self) -> TokType {
        self.peek().ty
    }

    // fn expect_match(&mut self, match_for)

    /// Parses current value at self.peek() as a ZIdent, and then
    /// advances cursor + 1, if not an identifer, returns error
    fn expect_rune(&mut self) -> anyhow::Result<AstRune> {
        let ty = self.peek().ty;
        if matches!(ty, TokType::Ident) {
            let ident = self.peek().clone().into_ast_rune();
            self.adv(1)?;
            Ok(ident)
        } else {
            bail!("Expected Identifier. Got: {:?}", ty)
        }
    }

    fn parameter_list(&mut self) -> anyhow::Result<AstList<Binding>> {
        let mut params = Vec::new();
        if matches!(self.peek_ty(), TokType::CloseParen) {
            return Ok(AstList::Nil);
        }

        let ident = self.expect_rune()?;
        let bi = Binding::new(ident);
        params.push(bi);

        while !self.is_eof() && self.peek().ty == TokType::Comma {
            self.adv(1)?;
            let ident = self.expect_rune()?;
            params.push(Binding::new(ident));
        }
        ensure!(
            matches!(self.peek().ty, TokType::CloseParen),
            "Expected ')' at end of parameter list. Got: {:?}",
            self.peek().clone()
        );
        let res = AstList::new(params);
        Ok(res)
    }

    fn loop_expr(&mut self) -> anyhow::Result<ExprStmt> {
        let loop_body = block!(self, TokType::Do | TokType::Begin, TokType::End)?;
        self.adv(1)?;
        let expr = ExprStmt::loop_block(loop_body);
        Ok(expr)
    }

    fn while_expr(&mut self) -> anyhow::Result<ExprStmt> {
        let cond = self.expression()?;

        let loop_body = block!(self, TokType::Do | TokType::Begin, TokType::End)?;
        self.adv(1)?;
        let expr = ExprStmt::while_block(cond, loop_body);
        Ok(expr)
    }

    fn if_expr(&mut self) -> anyhow::Result<ExprStmt> {
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
            branches.push(WhenForm::branch(cond, body));

            match self.peek().ty {
                TokType::End => {
                    self.adv(1)?;
                    branches.push(WhenForm::End);
                    break;
                }
                TokType::Else => {
                    let else_body = block!(self, TokType::Else, TokType::End)?;
                    self.adv(1)?;
                    branches.push(WhenForm::Else(ExprStmt::block_elide(else_body)));
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
        let branches = AstList::new(branches);
        let form = ExprStmt::When(branches);
        Ok(form)
    }

    fn bind_expr(&mut self, var_ty: BindType) -> anyhow::Result<ExprStmt> {
        if let TokType::Ident = self.peek().ty {
            let name = self.peek().clone();
            self.adv(1)?;
            let initializer = if let TokType::Eq = self.peek().ty {
                self.adv(1)?;
                Some(self.expression()?) // expression()?)
            } else {
                None
            };
            let name = AstRune::from(name.lexeme);
            let init = initializer.clone();
            let init = init.unwrap_or(Expr::Unit);
            let e = ExprStmt::binding(name, init, var_ty);
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
    fn when_expr(&mut self) -> anyhow::Result<ExprStmt> {
        let mut branches: Vec<WhenForm> = Vec::new();
        while !self.is_eof() && !matches!(self.peek().ty, TokType::End) {
            if matches!(self.peek().ty, TokType::Else) {
                self.adv(1)?;

                let otherwise = self.place_value_block(BlockType::WhenElse)?; //block!(self, TokType::ArrowRight, TokType::End | TokType::Comma)?;

                branches.push(WhenForm::Else(otherwise));
                println!("Matched Else: {:?}", self.peek().ty);
                break;
            } else {
                let cond = self.expression()?;
                let then = self.place_value_block(BlockType::WhenBranch)?;
                branches.push(WhenForm::Branch(cond, then));
            }
            // self.adv(1)?;
            // branches.push(WhenForm::Branch(cond,, then.into_expr()));
        }
        println!("End of when");
        branches.push(WhenForm::End);
        let when = ExprStmt::when_block(branches);
        Ok(when)
        // while self.peek().ty != TokType::End && !self.is_eof() {
        //     if matches!(self.peek().ty, TokType::Else) {
        //
        //         self.adv(1)?;
        //         ensure!(
        //             matches!(self.peek().ty, TokType::FatArrow),
        //             "Expected '=>' after 'else' in when expression"); self.adv(1)?; let then = self.place_value_block()?;
        //         // // self.adv(1)?;
        //         // // if !then.is_block() {
        //         //
        //         //     // ensure!(self.peek().ty == TokType::Comma,"require trailing comma if then branch is anything but a block.");
        //         //      // self.adv(1)?;
        //         // // }
        //         branches.push(WhenForm::Else(then));
        //         branches.push(WhenForm::End);
        //
        //     } else {
        //         let when_cond = self.expression()?;
        //         ensure!(
        //             matches!(self.peek().ty, TokType::FatArrow),
        //                 "Parse Error: '=>' required in when expressions in between branch left hand conditions and right hand blocks/exprs. Ex: cond_expr => block end",
        //
        //         );
        //         self.adv(1)?;
        //         let then = self.place_value_block()?;
        //
        //         if !then.is_block() {
        //             // require trailing comma if then branch is anything but a block.
        //             ensure!(self.peek().ty == TokType::Comma,"require trailing comma if then branch is anything but a block.");
        //              self.adv(1)?;
        //         }
        //         branches.push(WhenForm::Branch(when_cond, then));
        //     }
        // }

        // let w = Expr::when_form(branches);
        // Ok(w)
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
    ///  DOES NOT CALL self.adv(1)?;
    ///
    /// TODO: Implement tracking if expression is place or value.
    ///
    fn place_value_block(&mut self, ty: BlockType) -> anyhow::Result<ExprStmt> {
        let bl = match ty {
            BlockType::WhenBranch => {
                try_block!(self, TokType::ArrowRight, TokType::Comma)
            }
            BlockType::WhenElse => {
                let otherwise =
                    try_block!(self, TokType::ArrowRight, TokType::End | TokType::Comma);
                if matches!(self.peek().ty, TokType::Comma) {
                    //ignore trailing comma
                    self.adv(1)?;
                }

                ensure!(
                    matches!(self.peek().ty, TokType::End),
                    "Mismatched end token for when expression."
                );

                otherwise
            }
            BlockType::Do | BlockType::Begin => {
                try_block!(self, TokType::Begin | TokType::Do, TokType::End)
            }
        };

        if let Some(bl) = bl {
            self.adv(1)?;
            Ok(ExprStmt::block(bl))
        } else {
            let e = self.expression()?;
            Ok(ExprStmt::Atom(e))
        }
    }

    fn print_expr(&mut self, ty: PrintType) -> anyhow::Result<ExprStmt> {
        let expr = self.expression()?;

        let head = match ty {
            PrintType::Fmt => Rc::from("print"),
            PrintType::Newline => Rc::from("println"),
        };
        let call = Expr::call_slice(head, &[expr]);
        let call = ExprStmt::Atom(call);

        Ok(call)
    }

    fn is_terminal(&self) -> bool {
        matches!(self.peek().ty, TokType::Semicolon | TokType::NewLine)
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
                    let operator = self.peek_prev().clone().into_ast_rune();
                    let right = self.logical_bitwise()?;

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
            Ok(Expr::assignment(expr, value))
            // if let Some(sym) = expr.inner_ident() {
            //     Ok(ExprStmt::assignment(sym.clone(), value.clone()))
            // } else {
            //     bail!("{}", ParseError::InvalidAssignment)
            // }
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
                    let operator = self.peek_prev().clone().into_ast_rune();
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
                    let operator = self.peek_prev().clone().into_ast_rune();
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
                    let operator = self.peek_prev().clone().into_ast_rune();
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
                let operator = self.peek_prev().clone().into_ast_rune();
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
                Ok(Expr::Bool(false))
            }
            LexTok::Bool(true) => {
                self.adv(1)?;
                Ok(Expr::Bool(true))
            }
            LexTok::Nil => {
                self.adv(1)?;
                Ok(Expr::Unit)
            }
            LexTok::Number(ref n) => {
                let n = *n;
                self.adv(1)?;
                Ok(Expr::Float(n))
            }
            LexTok::String(ref s) => {
                let s = s.clone();
                let s = s.trim_matches('"');
                self.adv(1)?;
                Ok(Expr::String(Rc::from(s)))
            }
            LexTok::OpenParen => {
                self.adv(1)?;
                let expr = self.expression_stmt()?;
                if self.peek().ty == TokType::CloseParen {
                    self.adv(1)?;
                    Ok(expr.into_atom_expr())
                } else {
                    Err(anyhow::anyhow!(
                        "Expected matching ending right parenthesis in expression"
                    ))
                }
            }
            LexTok::Ident => {
                let name = self.peek().clone();
                self.adv(1)?;
                Ok(Expr::rune(name.to_string().as_ref()))
            }
            _ => {
                bail!("Unexpected token: {}", self.peek().to_string());
                // self.adv(1)?;
                // self.eat_terminals()?;
                // an error somewhere, retry.
                // let expr = self.expression_stmt()?;

                // Ok(expr)
            }
        }
    }

    fn equality(&mut self) -> anyhow::Result<Expr> {
        let mut expr = self.comparison()?;
        loop {
            match self.peek().ty {
                TokType::BangEq | TokType::EqEq => {
                    self.adv(1)?;
                    let operator = self.peek_prev().clone().into_ast_rune();

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

    fn peek_next(&self) -> Option<&Tok> {
        let i = self.i + 1;
        if i >= self.tokens.len() {
            None
        } else {
            Some(&self.tokens[i])
        }
    }

    #[inline]
    pub fn peek(&self) -> &Tok {
        &self.tokens[self.i]
    }

    #[inline]
    pub fn peek_prev(&self) -> &Tok {
        &self.tokens[self.i - 1]
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PrintType {
    Fmt,
    Newline,
}
