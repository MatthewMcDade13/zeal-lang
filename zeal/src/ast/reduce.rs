pub trait AstReducer<AstNode, Ret = AstNode> {
    fn reduce(&self, ast: &AstNode) -> Ret;
}
