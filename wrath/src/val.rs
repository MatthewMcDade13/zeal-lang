use std::{
    cell::{Cell, RefCell},
    collections::{hash_map::Iter, HashMap},
    fmt::Display,
    ops::{Deref, DerefMut},
    rc::Rc,
};

pub type CoreFn = fn(&[Value]) -> Value; // Rc<dyn Fn(&[Value]) -> Value>;
pub type WVec = Rc<[Value]>;
pub type WHashMap = Rc<HashMap<String, Value>>;
pub type WString = Rc<str>;

// #[derive(Debug, Clone)]
// pub struct WHashMap {
//     inner: Rc<RefCell<HashMap<String, Value>>>,
// }
//
// impl Default for WHashMap {
//     fn default() -> Self {
//         Self {
//             inne+r: Rc::new(RefCell::new(HashMap::new())),
//         }
//     }
// }
//
// impl WHashMap {
//     pub fn new(hm: HashMap<String, Value>) -> Self {
//         Self {
//             inner: Rc::new(RefCell::new(hm)),
//         }
//     }
// }
// #[derive(Default, Clone)]
// pub struct WVec {
//     inner: Rc<Vec<Cell<Value>>>,
// }

#[derive(Debug, Default, Clone)]
pub enum Value {
    List(WVec),
    Array(WVec),
    Number(f64),
    Str(WString),
    Bool(bool),
    Hashmap(WHashMap),
    Symbol(WString),
    /// Like symbol, but prefixed with ':'
    Atom(WString),
    // Func(fn(&[Value]) -> Value),
    Error(ErrVal),
    #[default]
    Nil,
}

#[derive(Default, Debug, Clone, Copy)]
pub enum ErrType {
    IndexOutOfRange,
    NumParams,
    NumParse,
    #[default]
    Unknown,
}

#[derive(Default, Debug, Clone)]
pub struct ErrVal {
    pub msg: String,
    pub loc: (u32, u32),
    pub ty: ErrType,
}

impl From<RefCell<Value>> for Value {
    fn from(value: RefCell<Value>) -> Self {
        value.into_inner()
    }
}

impl Value {
    pub fn into_mut(self) -> RefCell<Self> {
        RefCell::new(self)
    }

    #[inline]
    pub fn list() -> Self {
        let v = Rc::new([Value::Nil]);

        Self::List(v)
    }

    #[inline]
    pub fn array() -> Self {
        let v = Rc::new([Value::Nil]);
        Self::Array(v)
    }

    #[inline]
    pub fn hashmap() -> Self {
        let hm = Rc::new(HashMap::new());
        Self::Hashmap(hm)
    }

    pub fn error(msg: &str, ty: ErrType) -> Self {
        Self::Error(ErrVal {
            msg: msg.to_owned(),
            ty,
            ..Default::default()
        })
    }

    pub const fn as_num(&self) -> Option<f64> {
        match self {
            Value::Number(n) => Some(*n),
            _ => None,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::List(l) => {
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
            Value::Error(err) => write!(f, "{:?}", err),
        }
    }
}
