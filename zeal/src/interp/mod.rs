pub mod env;
pub mod eval;

use crate::{
    ast::{self, Ast, AstError, Expr},
    core_types::val::ZValue,
};
use anyhow::*;
use env::Env;

#[derive(Debug)]
pub struct Interpreter {
    env: Env,
    ast: Ast,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {
            env: Env::new(),
            ast: Ast::empty(),
        }
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn eval_file(&mut self, path: &str) -> anyhow::Result<ZValue> {
        let ast = Ast::from_file(path)?;
        self.eval_ast(ast)
    }

    pub fn eval(&mut self, expr: &Expr) -> anyhow::Result<ZValue> {
        match expr {
            Expr::Call(cl) => todo!(),
            Expr::Block(bl) => todo!(),
            Expr::List(li) => self.eval(li.as_ref()),
            Expr::Atom(at) => Ok(at.clone()),
        }

        // stmt.walk(self)
    }

    pub fn eval_ast(&mut self, ast: Ast) -> anyhow::Result<ZValue> {
        let exprs = ast.slice();
        let back = exprs.len() - 1;
        for expr in &exprs[..back] {
            let _ = self.eval(expr)?;
        }
        self.eval(&exprs[back])
    }

    fn eval_call_list(&mut self, cl: &[Expr]) -> anyhow::Result<ZValue> {
        let head = cl.first().expect("Cannot apply empty call list!");
        let mut evaled = Vec::with_capacity(8);

        if let Some(val) = head.as_zval_sym() {
            if let ZValue::Sym(s) = val {
                evaled.push(val.clone());

                for expr in &cl[1..] {
                    let zv = self.eval(expr)?;
                    // evaled.push(zv);
                }

                // lookup symbol in environment and execute it as a function with the
                // rest of the exprs
                // let func = self.env.get(s);
                // func(...&cl[1..])
            }

            todo!();
        } else {
            bail!("Head of call list expected to be symbol or atom. was: {head:?}");
        }
    }

    // pub fn execute_block(&mut self, statements: &[Stmt]) -> anyhow::Result<()> {
    //     self.env.push_scope(Scope::default());
    //     for stmt in statements {
    //         if let Err(e) = self.execute(stmt) {
    //             self.env.pop_scope();
    //             bail!("{}", e);
    //         };
    //     }
    //     self.env.pop_scope();
    //     Ok(())
    // }

    // pub fn eval(&mut self, expr: &ast::Ast) -> anyhow::Result<Value> {
    //     expr.walk(self)
    // }
}
//
// // TODO :: Refactor these eval_* functions into a single macro that can print out this code, or at
// // least define the eval_* functions with highly similar function bodies
// pub fn eval_minus(minus_op: &Token, value: &Value) -> anyhow::Result<Value> {
//     let num = value.as_number().map_err(|e| AstWalkError::RuntimeError {
//         token: minus_op.clone(),
//         message: format!("Operator must be a number, {}", e),
//     })?;
//     Ok(Value::Number(-num))
// }
//
// pub fn eval_le(left: &Value, operator: &Token, right: &Value) -> anyhow::Result<Value> {
//     match left {
//         Value::Number(ln) => {
//             let rn = right.as_number().map_err(|e| AstWalkError::RuntimeError {
//                 token: operator.clone(),
//                 message: format!(
//                     "mismatched less-than-equal operator: '{} < {}', {}",
//                     left.type_string(),
//                     right.type_string(),
//                     e
//                 ),
//             })?;
//             Ok(Value::Boolean(ln < &rn))
//         }
//         _ => bail!(
//             "{}",
//             AstWalkError::RuntimeError {
//                 token: operator.clone(),
//                 message: format!(
//                     "Lefthand side of multiplication operator must be a number, got: {}",
//                     left.type_string(),
//                 )
//             }
//         ),
//     }
// }

// pub fn eval_lt(left: &Value, operator: &Token, right: &Value) -> anyhow::Result<Value> {
//     match left {
//         Value::Number(ln) => {
//             let rn = right.as_number().map_err(|e| AstWalkError::RuntimeError {
//                 token: operator.clone(),
//                 message: format!(
//                     "mismatched less-than operator: '{} < {}', {}",
//                     left.type_string(),
//                     right.type_string(),
//                     e
//                 ),
//             })?;
//             Ok(Value::Boolean(ln > &rn))
//         }
//         _ => bail!(
//             "{}",
//             AstWalkError::RuntimeError {
//                 token: operator.clone(),
//                 message: format!(
//                     "Lefthand side of multiplication operator must be a number, got: {}",
//                     left.type_string(),
//                 )
//             }
//         ),
//     }
// }
//
// pub fn eval_ge(left: &Value, operator: &Token, right: &Value) -> anyhow::Result<Value> {
//     match left {
//         Value::Number(ln) => {
//             let rn = right.as_number().map_err(|e| AstWalkError::RuntimeError {
//                 token: operator.clone(),
//                 message: format!(
//                     "mismatched greater-than-equal operator: '{} >= {}', {}",
//                     left.type_string(),
//                     right.type_string(),
//                     e
//                 ),
//             })?;
//             Ok(Value::Boolean(ln >= &rn))
//         }
//         _ => bail!(
//             "{}",
//             AstWalkError::RuntimeError {
//                 token: operator.clone(),
//                 message: format!(
//                     "Lefthand side of multiplication operator must be a number, got: {}",
//                     left.type_string(),
//                 )
//             }
//         ),
//     }
// }
//
// pub fn eval_gt(left: &Value, operator: &Token, right: &Value) -> anyhow::Result<Value> {
//     match left {
//         Value::Number(ln) => {
//             let rn = right.as_number().map_err(|e| AstWalkError::RuntimeError {
//                 token: operator.clone(),
//                 message: format!(
//                     "mismatched greater-than operator: '{} > {}', {}",
//                     left.type_string(),
//                     right.type_string(),
//                     e
//                 ),
//             })?;
//             Ok(Value::Boolean(ln > &rn))
//         }
//         _ => bail!(
//             "{}",
//             AstWalkError::RuntimeError {
//                 token: operator.clone(),
//                 message: format!(
//                     "Lefthand side of multiplication operator must be a number, got: {}",
//                     left.type_string(),
//                 )
//             }
//         ),
//     }
// }
//
// pub fn eval_mul(left: &Value, operator: &Token, right: &Value) -> anyhow::Result<Value> {
//     match left {
//         Value::Number(ln) => {
//             let rn = right.as_number().map_err(|e| AstWalkError::RuntimeError {
//                 token: operator.clone(),
//                 message: format!(
//                     "mismatched multiplication operator: '{} * {}', {}",
//                     left.type_string(),
//                     right.type_string(),
//                     e
//                 ),
//             })?;
//             Ok(Value::Number(ln * rn))
//         }
//         _ => bail!(
//             "{}",
//             AstWalkError::RuntimeError {
//                 token: operator.clone(),
//                 message: format!(
//                     "Lefthand side of multiplication operator must be a number, got: {}",
//                     left.type_string(),
//                 )
//             }
//         ),
//     }
// }
//
// pub fn eval_div(left: &Value, operator: &Token, right: &Value) -> anyhow::Result<Value> {
//     match left {
//         Value::Number(ln) => {
//             let rn = right.as_number().map_err(|e| AstWalkError::RuntimeError {
//                 token: operator.clone(),
//                 message: format!(
//                     "mismatched division operator: '{} / {}', {}",
//                     left.type_string(),
//                     right.type_string(),
//                     e
//                 ),
//             })?;
//             Ok(Value::Number(ln / rn))
//         }
//         _ => bail!(
//             "{}",
//             AstWalkError::RuntimeError {
//                 token: operator.clone(),
//                 message: format!(
//                     "Lefthand side of division operator must be a number, got: {}",
//                     left.type_string(),
//                 )
//             }
//         ),
//     }
// }
//
// pub fn eval_sub(left: &Value, operator: &Token, right: &Value) -> anyhow::Result<Value> {
//     match left {
//         Value::Number(ln) => {
//             let rn = right.as_number().map_err(|e| AstWalkError::RuntimeError {
//                 token: operator.clone(),
//                 message: format!(
//                     "mismatched subtraction operator: '{} - {}', {}",
//                     left.type_string(),
//                     right.type_string(),
//                     e
//                 ),
//             })?;
//             Ok(Value::Number(ln - rn))
//         }
//         _ => bail!(
//             "{}",
//             AstWalkError::RuntimeError {
//                 token: operator.clone(),
//                 message: format!(
//                     "Lefthand side of subtraction operator must be a number, got: {}",
//                     left.type_string(),
//                 )
//             }
//         ),
//     }
// }
//
// pub fn eval_plus(left: &Value, operator: &Token, right: &Value) -> anyhow::Result<Value> {
//     match left {
//         Value::Number(ln) => {
//             let rn = right.as_number().map_err(|e| AstWalkError::RuntimeError {
//                 token: operator.clone(),
//                 message: format!(
//                     "mismatched addition operator: '{} + {}', {}",
//                     left.type_string(),
//                     right.type_string(),
//                     e
//                 ),
//             })?;
//             Ok(Value::Number(ln + rn))
//         }
//         Value::Obj(obj) => match obj {
//             Object::String(ls) => {
//                 let rs = right.as_string().map_err(|e| AstWalkError::RuntimeError {
//                     token: operator.clone(),
//                     message: format!(
//                         "mismatched addition operator: '{} + {}', {}",
//                         left.type_string(),
//                         right.type_string(),
//                         e
//                     ),
//                 })?;
//                 Ok(Value::Obj(Object::String(ls.clone() + &rs)))
//             }
//         },
//
//         _ => todo!(),
//     }
// }
