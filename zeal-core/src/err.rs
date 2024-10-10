// #[derive(Debug, Clone, Copy)]
// pub struct ErrorLine {
//     line: usize,
//     col: usize,
// }
//
// #[derive(Debug, Clone)]
// pub struct ZealErr<T> {
//     pub error: T,
//     pub line: ErrorLine,
//     pub message: Option<String>,
// }
//
// pub mod parse {
//     use std::fmt::Display;
//
//     #[derive(Default, Debug, Clone)]
//     pub struct ExprInfo {
//         pub ty: String,
//         pub string: ExprString,
//     }
//
//     #[derive(Debug, Clone)]
//     pub struct ParseErrInfo {
//         pub expr: Option<ExprInfo>,
//         pub prev: Tok,
//         pub curr: Tok,
//         pub next: Option<Tok>,
//     }
//
//     impl Display for ParseErrInfo {
//         fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//             let LineInfo { line, col } = self.curr.info;
//
//             let (expr, expr_ty) = if let Some(ex) = &self.expr {
//                 (ex.string.as_str(), ex.ty.as_str())
//             } else {
//                 ("n/a", "n/a")
//             };
//
//             let prev = &self.prev;
//             let curr = &self.curr;
//             let next = if let Some(n) = &self.next {
//                 n.to_string()
//             } else {
//                 "None".into()
//             };
//
//             write!(f, "At => (L:{line}|C:{col})\n\t-> While parsing expression: {expr}\n\t -> of type: {expr_ty}.\n\t-> Current: {curr}\n\t-> Prev: {prev}\n\t-> Next: {next}")
//         }
//     }
//
//         #[default]
//         #[error("Unknown parse error occured.")]
//         Unknown,
//     }
// }
//
// pub mod lex {
//     use std::num::ParseIntError;
//
// pub mod core {
//     use std::collections::HashMap;
//
//     use crate::{
//         ast::VarType,
//         compiler::{
//             opcode::{Op, Opcode, VarOp},
//             state::Scope,
//         },
//         core_types::{str::ZIdent, val::ZValue},
//     };
//
//     #[derive(Debug, thiserror::Error, Clone)]
//     pub enum CompileError {
//         #[error("Unresolved local identifier: {name}. Compiling Op: {op}, locals: {scope_state}")]
//         UnresolvedLocal {
//             name: ZIdent,
//             op: Op,
//             scope_state: Scope,
//         },
//         #[error("Invalid Assigment on Identifier: {name}, var_op: {op_ty} ")]
//         InvalidAssignment { op_ty: VarOp, name: ZIdent },
//     }
//
//     #[derive(Debug, thiserror::Error, Clone)]
//     pub enum RuntimeError {
//         #[error("Invalid type: expected: {expected_type}; got: {actual_type}. Inside function: {in_fn:?}. Message: {message:?}" )]
//         InvalidType {
//             expected_type: &'static str,
//             actual_type: &'static str,
//             in_fn: Option<&'static str>,
//             message: Option<String>,
//         },
//         #[error(
//             "Unknown Identifier: {name}, encountered from opcode: {opcode:?}, vm constants: {constants:?}, globals: {globals:?}"
//         )]
//         VMUnknownIdentifier {
//             name: ZIdent,
//             opcode: Opcode,
//             constants: Vec<ZValue>,
//             globals: HashMap<ZIdent, ZValue>,
//         },
//     }
// }
