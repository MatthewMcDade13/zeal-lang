pub mod ast;
pub mod core_types;
pub mod env;
pub mod err;
pub mod interp;
pub mod lex;
pub mod mem;
pub mod parse;
pub mod sys;

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
