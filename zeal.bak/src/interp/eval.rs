use anyhow::bail;

use crate::{
    core_types::{num::ZFloat64, str::ZString, val::ZValue},
    err::core::RuntimeError,
};

// NOTE:: We are going to type-check at runtime here, (or maybe after doing a quick typecheck pass?)
// Why you ask? I decided on going with static tying, though i still do want to have a repl
// tree-walking interpreter so i can just exec code on the fly, though to be honest, JIT would prob
// be a lot faster.... lol idk, typecheck at runtime it is. /rant

pub struct CoreFn<T>(pub(crate) T)
where
    T: Fn(&[ZValue]) -> ValResult;

pub type ValResult = anyhow::Result<ZValue>;

macro_rules! eval_num_binary {
    ($fn_name: ident, $op: tt, $init: expr) => {
        pub fn $fn_name(vals: &[ZValue]) -> ValResult {
            let mut acc = ZFloat64::new($init);
            for v in vals {
                if let ZValue::Number(n) = v {
                    acc = acc * *n;
                } else {
                    bail!(RuntimeError::InvalidType {
                        expected_type: "Number".into(),
                        actual_type: v.type_string().into(),
                        in_fn: None,
                        message: None,
                    })
                }
            }
            let n = ZValue::Number(acc);
            Ok(n)
        }
    };
}

eval_num_binary!(mul_num, *, 1.0);
eval_num_binary!(div_num, /, 1.0);
eval_num_binary!(add_nun, +, 0.0);
eval_num_binary!(sub_num, -, 0.0);

pub fn concat_string(vals: &[ZValue]) -> ZValue {
    let mut acc = String::new();
    for v in vals {
        let vs = v.expect_string();
        let vs = vs.as_str();
        acc.push_str(vs);
    }

    ZValue::string(ZString::from(acc))
}

// use crate::core_types::val::ZValue;
//
//
// pub fn add(vals: &[ZValue]) -> anyhow::Result<ZValue> {
//     let mut acc =
//     for v in vals {
//         if let Some(n) = v {
//
//         }
//     }
// }
