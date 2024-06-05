use std::{collections::HashMap, fmt::Display, rc::Rc};

#[derive(Debug, Default, Clone)]
pub enum Value {
    List(Vec<Value>),
    Array(Vec<Value>),
    Number(f64),
    Str(String),
    Bool(bool),
    Hashmap(HashMap<String, Value>),
    Symbol(String),
    /// Like symbol, but prefixed with ':'
    Atom(String),
    // Func(fn(&[Value]) -> Value),
    #[default]
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::List(l) => {
                // let mut s = String::with_capacity(l.len());

                let s = l
                    .iter()
                    .map(|i| i.to_string())
                    .intersperse(" ".into())
                    .collect::<String>();
                write!(f, "({})", s)
            }
            Value::Array(l) => {
                let s = l
                    .iter()
                    .map(|i| i.to_string())
                    .intersperse(" ".into())
                    .collect::<String>();
                write!(f, "[{}]", s)
            }

            Value::Number(n) => {
                write!(f, "{}", n)
            }
            Value::Str(s) => write!(f, "\"{}\"", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Hashmap(m) => {
                let mut sb = String::with_capacity(m.len() * 2);
                for (k, v) in m.iter() {
                    // TODO :: atoms print an extra ':'
                    let s = format!("{}: {},", k, v);
                    sb.push_str(&s);
                }
                write!(f, "{{ {sb} }}")
            }
            Value::Symbol(s) => write!(f, "{}", s),
            Value::Atom(a) => write!(f, ":{}", a),
            Value::Nil => write!(f, "nil"),
        }
    }
}
