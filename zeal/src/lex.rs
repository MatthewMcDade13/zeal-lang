use std::{collections::HashMap, hash::Hash};

use anyhow::{anyhow, Ok};
use logos::{Lexer, Logos, Skip};

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexError {
    InvalidSymbol(String),
    #[default]
    IllegalCharacter,
}

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    Str(String),
    Array(Vec<Value>),
    Obj(HashMap<String, Value>),

    // No-op / whitespace placeholder (for parsing)
    Void,
}

#[derive(Debug, Clone, Logos)]
#[logos(skip r"[ \t\r\f]+")]
#[logos(extras = (usize, usize))]
#[logos(error = String)]
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
                Token::NewLine => Value::Void,
                Token::Word(_) => Value::Void,
                Token::Bool(b) => Value::Bool(b),
                Token::BraceOpen => parse_obj(lex)?,
                // Token::BraceClosed => todo!(),
                Token::BracketOpen => parse_array(lex)?,
                // Token::BracketClose => todo!(),
                Token::Colon => todo!(),
                Token::Comma => todo!(),
                Token::Nil => Value::Nil,
                Token::Number(n) => Value::Number(n),
                Token::String(s) => Value::Str(s),

                _ => {
                    anyhow::bail!("Unexpected token at {}:{}", lex.extras.0, lex.extras.1);
                }
            },
            Err(_) => {
                anyhow::bail!("Unexpected token at {}:{}", lex.extras.0, lex.extras.1);
            }
        };
        anyhow::Result::Ok(val)
    } else {
        anyhow::bail!(
            "Empty values are invalid. token at {}:{}",
            lex.extras.0,
            lex.extras.1
        )
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
                    return Ok(Value::Array(arr));
                }
                // Token::Colon => todo!(),
                Token::Nil => {
                    arr.push(Value::Nil);
                }
                Token::Number(n) => {
                    arr.push(Value::Number(n));
                }
                Token::String(s) => arr.push(Value::Str(s)),
                _ => {
                    anyhow::bail!("Unexpected token at: {}:{}", lex.extras.0, lex.extras.1)
                }
            }
        }
    }
    anyhow::bail!("Unmatched open bracket defined here: {:?}", lex.span())
}

fn parse_obj(lex: &mut Lexer<Token>) -> anyhow::Result<Value> {
    let mut map = HashMap::new();
    let span = lex.span();
    let mut wait_key = false;
    let mut wait_comma = false;

    while let Some(tok) = lex.next() {
        if let std::result::Result::Ok(token) = tok {
            match token {
                Token::BraceClosed if !wait_key => return Ok(Value::Obj(map)),
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

                        anyhow::bail!(s);
                    }
                },
                _ => {
                    anyhow::bail!("unexpected token here: {}:{}", lex.extras.0, lex.extras.1);
                }
            }
        }
        wait_comma = !wait_key;
    }
    anyhow::bail!(
        "unmatched opening brace defined here: {}:{}",
        lex.extras.0,
        lex.extras.1
    );
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
