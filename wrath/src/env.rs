use std::{collections::HashMap, rc::Rc};

use crate::val::{CoreFn, Value};

#[derive(Default, Clone)]
pub struct Env {
    outer: Option<Box<Self>>,
    data: HashMap<String, CoreFn>,
}

impl Env {
    pub fn core() -> Self {
        let mut hm: HashMap<String, CoreFn> = HashMap::new();
        hm.insert("+".into(), core::eval_add);
        hm.insert("-".into(), core::eval_sub);
        hm.insert("*".into(), core::eval_mul);
        hm.insert("/".into(), core::eval_div);
        Self {
            outer: None,
            data: hm,
        }
    }
}

pub mod core {
    use crate::val::{ErrType, Value};

    pub fn eval_add(vs: &[Value]) -> Value {
        if vs.len() < 1 {
            Value::error("Cannot eval '+ with no parameters.", ErrType::NumParams)
        } else {
            let mut acc = 0.0;
            for v in vs.iter() {
                if let Some(n) = v.as_num() {
                    acc += n;
                } else {
                    return Value::error(
                        &format!("Cannot add value: {} as a number", v),
                        ErrType::NumParse,
                    );
                };
            }
            Value::Number(acc)
        }
    }

    pub fn eval_sub(vs: &[Value]) -> Value {
        if vs.len() < 1 {
            Value::error("Cannot eval '- with no parameters.", ErrType::NumParams)
        } else {
            let mut acc = 0.0;
            for v in vs.iter() {
                if let Some(n) = v.as_num() {
                    acc -= n;
                } else {
                    return Value::error(
                        &format!("Cannot sub value: {} as a number", v),
                        ErrType::NumParse,
                    );
                };
            }
            Value::Number(acc)
        }
    }

    pub fn eval_mul(vs: &[Value]) -> Value {
        if vs.len() < 1 {
            Value::error("Cannot eval '* with no parameters.", ErrType::NumParams)
        } else {
            let mut acc = 1.0;
            for v in vs.iter() {
                if let Some(n) = v.as_num() {
                    acc *= n;
                } else {
                    return Value::error(
                        &format!("Cannot multiply value: {} as a number", v),
                        ErrType::NumParse,
                    );
                };
            }
            Value::Number(acc)
        }
    }

    pub fn eval_div(vs: &[Value]) -> Value {
        if vs.len() < 1 {
            Value::error("Cannot '/ with no parameters.", ErrType::NumParams)
        } else {
            let mut acc = 1.0;
            for v in vs.iter() {
                if let Some(n) = v.as_num() {
                    acc /= n;
                } else {
                    return Value::error(
                        &format!("Cannot divide value: {} as a number", v),
                        ErrType::NumParse,
                    );
                };
            }
            Value::Number(acc)
        }
    }
}
