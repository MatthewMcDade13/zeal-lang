use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Clone, Copy)]
pub enum BasicType {
    Unit,
    Byte,
    Int8,
    UInt16,
    Int16,
    UInt32,
    Int32,
    UInt64,
    Int64,
    Ptr,
    Var,
}

#[derive(Debug, Clone)]
pub struct StructType {
    name: String,
    fields: HashMap<String, Type>,
}

// pub struct ValueType {}

#[derive(Debug, Clone)]
pub struct FuncType {
    name: String,
    params: Vec<Type>,
    ret: Rc<Type>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Basic(BasicType),
    Struct(StructType),
    Func(FuncType),
}
