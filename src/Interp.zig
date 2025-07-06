//! A tree-walking interpreter for the Zeal Programming Language
//! This is used when evaluating zeal files as scripts, thus running
//! zeal source code in interpreted mode. This mode skips the bytecode compile phase
//! and directly evaluates the source code.
//!
//! Also used by compiler backend for compile time evaluation.
//! Compile-Time evaluation is as of 07/04/2025, a feature that still tentative
