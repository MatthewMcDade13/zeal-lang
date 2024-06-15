use std::{cell::RefCell, collections::HashMap, hash::Hash, num::ParseIntError, rc::Rc};

// TODO :: MAKE THE FUCKING GC FIRST
//
use logos::{Lexer, Logos, Skip};

use crate::val::Value;

// type LexResult<T> = anyhow::Result<T, LexError>;

#[derive(thiserror::Error, Default, Debug, Clone, PartialEq)]
pub enum LexError {
    #[error("Invalid or Unknown Symbol: {0}")]
    InvalidSymbol(String),
    #[default]
    #[error("Illegal Character encountered")]
    IllegalCharacter,
    #[error("Error Parsing Invalid Number: {0}")]
    InvalidNumber(String),
}

impl From<std::num::ParseFloatError> for LexError {
    fn from(err: std::num::ParseFloatError) -> Self {
        LexError::InvalidNumber(err.to_string())
    }
}

impl From<ParseIntError> for LexError {
    fn from(err: ParseIntError) -> Self {
        use std::num::IntErrorKind::*;
        match err.kind() {
            PosOverflow | NegOverflow => LexError::InvalidNumber("overflow error".to_owned()),
            _ => LexError::InvalidNumber("other error".to_owned()),
        }
    }
}

#[derive(Debug, Clone, Logos)]
#[logos(skip r"[ \t\r\f]+")]
#[logos(extras = (usize, usize))]
#[logos(error = LexError)]
pub enum Token {
    #[regex(r"\n", newline_cb)]
    NewLine,

    #[regex(r"\w+", word_cb)]
    Word((usize, usize)),
    #[token("false", |_| false)]
    #[token("true", |_| true)]
    Bool(bool),

    #[token("{")]
    BraceOpen,

    #[token("}")]
    BraceClosed,

    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token("nil")]
    Nil,

    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", |lex| lex.slice().parse::<f64>().unwrap(), priority = 50)]
    Number(f64),

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| lex.slice().to_owned())]
    String(String),
}

pub fn parse_value(lex: &mut Lexer<'_, Token>) -> anyhow::Result<Value> {
    if let Some(tok) = lex.next() {
        let val = match tok {
            std::result::Result::Ok(tok) => match tok {
                Token::NewLine => Value::Unit,
                Token::Word(_) => Value::Unit,
                Token::Bool(b) => Value::Bool(b),
                Token::BraceOpen => parse_obj(lex)?,
                // Token::BraceClosed => todo!(),
                Token::BracketOpen => parse_array(lex)?,
                // Token::BracketClose => todo!(),
                Token::Colon => todo!(),
                Token::Comma => todo!(),
                Token::Nil => Value::Nil,
                Token::Number(n) => Value::Number(n),
                Token::String(s) => todo!(), // Value::Str(s),

                _ => {
                    anyhow::bail!(LexError::InvalidSymbol(format!(
                        "Unexpected token at {}:{}",
                        lex.extras.0, lex.extras.1
                    )));
                }
            },
            Err(_) => {
                anyhow::bail!(LexError::InvalidSymbol(format!(
                    "Unexpected token at {}:{}",
                    lex.extras.0, lex.extras.1
                )));
                // "Unexpected token at {}:{}", lex.extras.0, lex.extras.1);
            }
        };
        anyhow::Result::Ok(val)
    } else {
        anyhow::bail!(LexError::InvalidSymbol(format!(
            "Empty values are invalid. token at {}:{}",
            lex.extras.0, lex.extras.1
        )))
        //
        //     "Empty values are invalid. token at {}:{}",
        //     lex.extras.0,
        //     lex.extras.1
        // )
    }
}

fn parse_array(lex: &mut Lexer<Token>) -> anyhow::Result<Value> {
    let mut arr = Vec::with_capacity(24);
    let span = lex.span();

    while let Some(tok) = lex.next() {
        if let std::result::Result::Ok(token) = tok {
            match token {
                Token::NewLine | Token::Comma => {
                    continue;
                }
                Token::Bool(b) => {
                    arr.push(Value::Bool(b));
                }
                Token::BraceOpen => {
                    let obj = parse_obj(lex)?;
                    arr.push(obj);
                }
                // Token::BraceClosed => todo!(),
                Token::BracketOpen => {
                    let child_arr = parse_array(lex)?;
                    arr.push(child_arr);
                }
                Token::BracketClose => {
                    todo!()
                    // return anyhow::Result::Ok(Value::Array(arr));
                }
                // Token::Colon => todo!(),
                Token::Nil => {
                    arr.push(Value::Nil);
                }
                Token::Number(n) => {
                    arr.push(Value::Number(n));
                }
                Token::String(s) => todo!(), //arr.push(Value::Str(s)),
                _ => {
                    // "Unexpected token at: {}:{}", lex.extras.0, lex.extras.1)

                    anyhow::bail!(LexError::InvalidSymbol(format!(
                        "Unexpected token at {}:{}",
                        lex.extras.0, lex.extras.1
                    )));
                }
            }
        }
    }

    anyhow::bail!(LexError::InvalidSymbol(format!(
        "Unmatched open bracket defined here: {:?}",
        lex.span()
    )))
    // "Unmatched open bracket defined here: {:?}", lex.span())
}

fn parse_obj(lex: &mut Lexer<Token>) -> anyhow::Result<Value> {
    let mut map = HashMap::new();
    let span = lex.span();
    let mut wait_key = false;
    let mut wait_comma = false;

    while let Some(tok) = lex.next() {
        if let std::result::Result::Ok(token) = tok {
            match token {
                Token::BraceClosed if !wait_key => todo!(), //return anyhow::Result::Ok(Value::Obj(map)),
                Token::NewLine if !wait_key && !wait_comma => {
                    continue;
                }
                Token::Comma if wait_comma => {
                    wait_key = true;
                }
                Token::String(key) if !wait_comma => match lex.next() {
                    Some(std::result::Result::Ok(Token::Colon)) => {
                        let val = parse_value(lex)?;
                        map.insert(key, val);
                        wait_key = false;
                    }
                    _ => {
                        let s = format!("unexpected token here,({:?}) expected ':'", lex.span());

                        anyhow::bail!(LexError::InvalidSymbol(s));
                    }
                },
                _ => {
                    let s = format!("unexpected token here,({:?}) expected ':'", lex.span());

                    anyhow::bail!(LexError::InvalidSymbol(s));
                    // "unexpected token here: {}:{}", lex.extras.0, lex.extras.1);
                }
            }
        }
        wait_comma = !wait_key;
    }

    anyhow::bail!(LexError::InvalidSymbol(format!(
        "unmatched opening brace defined here: {}:{}",
        lex.extras.0, lex.extras.1
    )))
}

fn newline_cb(lex: &mut Lexer<Token>) -> Skip {
    lex.extras.0 += 1;
    lex.extras.1 = lex.span().end;
    Skip
}

fn word_cb(lex: &mut Lexer<Token>) -> (usize, usize) {
    let line = lex.extras.0;
    let col = lex.span().start - lex.extras.1;

    (line, col)
}
