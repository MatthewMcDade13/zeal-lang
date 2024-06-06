use crate::{env::Env, lex, val::Value};

pub fn read(ast: String) -> anyhow::Result<Value> {
    lex::read_str(ast.as_str())
    // let v = eval(v)?;
    // let vs = print(v)?;
}

pub fn eval(ast: Value, env: Env) -> anyhow::Result<Value> {
    Ok(ast)
}

pub fn print(ast: Value) -> anyhow::Result<String> {
    Ok(ast.to_string())
}

pub fn eval_ast(ast: Value, env: Env) -> anyhow::Result<Value> {}

pub fn rep(ast: String, env: Env) -> anyhow::Result<String> {
    let v = read(ast)?;
    let v = eval(v, env)?;
    let vs = print(v)?;
    Ok(vs)
}
