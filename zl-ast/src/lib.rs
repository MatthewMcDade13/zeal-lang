use lib0_rs::zedalloc::gen_alloc::GenAllocator;

pub mod err;
pub mod expr;
pub mod lex;
pub mod parse;
pub mod walk;

#[global_allocator]
static GLOBAL: GenAllocator = GenAllocator;
