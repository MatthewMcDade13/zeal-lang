use crate::val::Val;

#[no_mangle]
pub fn zvm_println(vals: &[Val]) -> anyhow::Result<Val> {
    let mut s = String::new();
    for v in vals.iter() {
        let ss = format!("{v} ");
        s.push_str(&ss);
    }
    let s = s.trim_end();
    println!("{s}");
    Ok(Val::Unit)
}
