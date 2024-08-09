pub mod ast;
pub mod compiler;
pub mod core_types;
pub mod err;
pub mod interp;
pub mod mem;
pub mod sys;
mod test;
pub mod vm;

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
