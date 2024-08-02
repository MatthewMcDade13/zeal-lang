use crate::core_types::{str::ZString, val::ZValue};

pub struct CoreFn<T>(T)
where
    T: Fn(&[ZValue]) -> ZValue;

macro_rules! eval_binary {
    ($fn_name: ident, $op: ident, $op_type: ident) => {
        pub fn $fn_name(vals: &[ZValue]) -> ZValue {
            let mut acc = $op_type::default();
            for v in vals {
                acc = acc $op  ;
            }
            ZValue::from(acc)
        }
    };
}

pub fn add_num(vals: &[ZValue]) -> ZValue {
    let mut acc = 0.0;
    for v in vals {
        let vn = *v.unwrap_float64();
        acc = acc + vn;
    }
    ZValue::number(acc)
}

pub fn concat_string(vals: &[ZValue]) -> ZValue {
    let mut acc = String::new();
    for v in vals {
        let vs = v.unwrap_string();
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
