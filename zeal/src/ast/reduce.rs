pub trait Reducer<AstNode, Ret> {
    fn reduce(&self, ast: &AstNode) -> Ret;
}
