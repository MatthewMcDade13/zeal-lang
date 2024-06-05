use regex::{Captures, Regex};

use crate::val::Value;

#[derive(Debug, Clone)]
pub struct Lexer {
    toks: Vec<String>,
    i: usize,
}

impl Lexer {
    pub fn new() -> Self {
        Self {
            toks: Vec::new(),
            i: 0,
        }
    }

    /// Peeks current token (self.toks(self.i)) and then incremnts self.i + 1
    pub fn peek_adv(&mut self) -> &str {
        assert!(
            self.i < self.toks.len(),
            "Index out of range. Index => {}, Len => {}",
            self.i,
            self.toks.len()
        );

        let s = &self.toks[self.i];
        self.i = self.i + 1;
        s
    }

    /// effectively :: self.toks(self.i + 1) (with bounds checks lol)
    pub fn adv(&mut self) -> &str {
        assert!(
            self.i < self.toks.len(),
            "Index out of range. Index => {}, Len => {}",
            self.i,
            self.toks.len()
        );

        self.i = self.i + 1;
        &self.toks[self.i]
    }

    /// Peeks current token at self.toks(self.i)
    pub fn peek(&self) -> &str {
        assert!(
            self.i < self.toks.len(),
            "Index out of range. Index => {}, Len => {}",
            self.i,
            self.toks.len()
        );
        &self.toks[self.i]
    }

    /// skips self.i ahead n
    pub fn skip(&mut self, n: u32) {
        self.i += n as usize;
    }

    pub fn read_form(&mut self) -> anyhow::Result<Value> {
        let tok = self.peek();
        match tok {
            "{" => {
                self.skip(1);
                todo!();
            }
            "[" => {
                self.skip(1);
                todo!();
            }
            "(" => {
                self.skip(1);
                self.read_list()
            }
            _ => self.read_atom(),
        }
    }

    pub fn read_atom(&mut self) -> anyhow::Result<Value> {
        let tok = self.peek().to_owned();
        if tok.starts_with("\"") {
            let s = String::from(&tok[1..(tok.len() - 1)]);
            Ok(Value::Str(s))
        } else if tok.starts_with("'") {
            Ok(Value::Atom(tok))
        } else {
            match tok.parse::<f64>() {
                Ok(n) => Ok(Value::Number(n)),
                Err(_) => match tok.as_str() {
                    "true" => Ok(Value::Bool(true)),
                    "false" => Ok(Value::Bool(false)),
                    "nil" => Ok(Value::Nil),
                    _ => Ok(Value::Symbol(tok)),
                },
            }
        }
    }

    pub fn read_list(&mut self) -> anyhow::Result<Value> {
        let mut list = Vec::new();
        loop {
            // assert!(
            //     self.current >= self.toks.len(),
            //     "read_list => Missing closing parenthesis."
            // );

            let tok = self.peek();

            if tok == ")" {
                break;
            }

            let v = self.read_form()?;
            list.push(v);
            self.skip(1);
        }
        Ok(Value::List(list))
    }
}

pub fn tokenize(src: &str) -> anyhow::Result<Vec<String>> {
    lazy_static::lazy_static! {
        static ref RE: Regex = Regex::new(
            r###"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]+)"###
        )
        .unwrap();
    }
    let mut toks = Vec::new();
    for cap in RE.captures_iter(src) {
        if cap[1].starts_with(";") {
            continue;
        }
        toks.push(String::from(&cap[1]));
    }
    Ok(toks)
}

pub fn read_str(src: &str) -> anyhow::Result<Value> {
    let toks = tokenize(src)?;
    let mut lex = Lexer { toks, i: 0 };
    lex.read_form()
}
