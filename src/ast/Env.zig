//! A self-contained Ast Environment. Includes
//! loaded Modules / AST Tree(s) along with Symbol tables
//! and other static (pre-evaluation) metadata
//!
//! Our frontend parser packages source code into this data structure where:
//! - it is then passed along to our by compiler backed to compile
//! - it is passed along to our interpreter for immediate eval

