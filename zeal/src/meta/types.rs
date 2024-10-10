use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use crate::core_types::{self, str::ZIdent, val::ZValue};

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    #[default]
    Any,
    /// u8
    Byte,
    /// Vec<u8> or equivalent
    Binary,
    /// isize
    Num,
    /// usize
    UNum,
    /// f64
    Float,
    String,
    Rune,
    /// Used for ZValue::Ident. Soon Rune === Ident
    RuneIdent,
    Vector,
    List,
    Map,
    Set,
    Ref,
    Struct,
    Trait,
    Unit,
    Option,
    Nil,
    Bool,
    Function,
    Closure,
    /// Type variable
    Var,
}

impl Type {
    /// Gets the mimmimm (smallest) size of self.
    /// this  is because we cant determine the size of things like Vec<T>.
    /// we can only return the size of the basic type with no generic parameters (ex: An empty Vec)
    pub const fn min_size_bytes(&self) -> usize {
        const PTR_SIZE: usize = std::mem::size_of::<usize>();
        const WORD_SIZE: usize = std::mem::size_of::<u32>();
        const FLOAT_SIZE: usize = std::mem::size_of::<f64>();

        match self {
            Type::Any => PTR_SIZE,
            Type::Byte => 1,
            Type::Binary => PTR_SIZE,
            Type::Num => WORD_SIZE,
            Type::UNum => WORD_SIZE,
            Type::Float => FLOAT_SIZE,
            Type::String => PTR_SIZE,
            Type::Rune => PTR_SIZE,
            Type::RuneIdent => PTR_SIZE,
            Type::Vector => PTR_SIZE,
            Type::List => PTR_SIZE,
            Type::Map => PTR_SIZE,
            Type::Set => PTR_SIZE,
            Type::Ref => PTR_SIZE,
            Type::Struct => 0,
            Type::Trait => 0,
            Type::Unit => 0,
            Type::Option => 0,
            Type::Nil => 0,
            Type::Bool => 1,
            Type::Function => PTR_SIZE,
            Type::Closure => PTR_SIZE,
            Type::Var => 1,
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeName {
    pub name: ZIdent,
    pub ty: Type,
}

impl From<Type> for TypeName {
    fn from(ty: Type) -> Self {
        let name = match ty {
            Type::Any => ZIdent::from("Any"),
            Type::Byte => ZIdent::from("Byte"),
            Type::Binary => ZIdent::from("Binary"),
            Type::Num => ZIdent::from("Num"),
            Type::UNum => ZIdent::from("UNum"),
            Type::Float => ZIdent::from("Float"),
            Type::String => ZIdent::from("String"),
            Type::Rune => ZIdent::from("Rune"),
            Type::RuneIdent => ZIdent::from("RuneIdent"),
            Type::Vector => ZIdent::from("Vector"),
            Type::List => ZIdent::from("List"),
            Type::Map => ZIdent::from("Map"),
            Type::Set => ZIdent::from("Set"),
            Type::Ref => ZIdent::from("MutRef"),
            Type::Struct => ZIdent::from("Struct"),
            Type::Trait => ZIdent::from("Trait"),
            Type::Unit => ZIdent::from("Unit"),
            Type::Option => ZIdent::from("Option"),
            Type::Nil => ZIdent::from("Nil"),
            Type::Bool => ZIdent::from("Boolean"),
            Type::Function => ZIdent::from("Function"),
            Type::Closure => ZIdent::from("Closure"),
            Type::Var => ZIdent::from("TypeVar"),
        };
        Self { name, ty }
    }
}

impl TypeName {
    /// Creates a Typename from given type, where typename.name is just the string name of given
    /// type ty
    pub fn basic(ty: Type) -> Self {
        Self::from(ty)
    }
}

#[derive(Debug, Clone, Eq, PartialOrd, Ord)]
pub struct Prototype {
    /// Function name is the same as its type
    pub typename: TypeName,
    pub params: Vec<Type>,
    pub ret: Type,
}

impl Prototype {
    pub fn as_shape(&self) -> ProtoShape {
        let typename = self.typename.clone();
        let arity = self.params.len();
        let receiver = if arity > 0 {
            self.params[0].clone()
        } else {
            Type::Unit
        };
        ProtoShape {
            typename,
            arity,
            receiver,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct ProtoShape {
    pub typename: TypeName,
    pub arity: usize,
    pub receiver: Type,
}

impl PartialEq<ProtoShape> for Prototype {
    fn eq(&self, other: &ProtoShape) -> bool {
        if self.params.len() == other.arity {
            if self.typename == other.typename {
                if self.params.len() > 0 {
                    let recv = &self.params[0];
                    recv == &other.receiver
                } else {
                    true
                }
            } else {
                false
            }
        } else {
            false
        }
    }
}

impl PartialEq for Prototype {
    fn eq(&self, other: &Self) -> bool {
        if self.params.len() == other.params.len() {
            if self.typename == other.typename {
                if self.params.len() > 0 {
                    let a = self.params.first().unwrap();
                    let b = other.params.first().unwrap();
                    a == b
                } else {
                    true
                }
            } else {
                false
            }
        } else {
            false
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
    /// Size of type in bytes
    pub size_bytes: usize,
    pub sym: TypedIdent,
}

impl Default for TypeInfo {
    fn default() -> Self {
        Self {
            size_bytes: Default::default(),
            sym: Default::default(),
        }
    }
}

impl ZIdent {
    pub const fn into_typed(self, ty: Type) -> TypedIdent {
        TypedIdent::Variable(TypeName { name: self, ty })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructShape {
    pub typename: TypeName,
    pub fields: HashMap<ZIdent, TypedIdent>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitShape {
    pub typename: TypeName,
    pub methods: HashMap<ZIdent, Prototype>,
}

/// Pre-runtime UniqueIdentifier
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedIdent {
    Variable(TypeName),
    Struct(StructShape),
    Trait(TraitShape),
    TypeVar(TypeName),
    Function(Prototype),
}

impl TypedIdent {
    pub const fn typename(&self) -> &TypeName {
        match self {
            TypedIdent::TypeVar(tn) | TypedIdent::Variable(tn) => tn,
            TypedIdent::Function(proto) => &proto.typename,
            TypedIdent::Struct(sh) => &sh.typename,
            TypedIdent::Trait(th) => &th.typename,
        }
    }
}

impl Default for TypedIdent {
    fn default() -> Self {
        Self::Variable(TypeName::from(Type::Any))
    }
}

impl From<TraitShape> for TypedIdent {
    fn from(v: TraitShape) -> Self {
        Self::Trait(v)
    }
}

impl From<StructShape> for TypedIdent {
    fn from(v: StructShape) -> Self {
        Self::Struct(v)
    }
}

impl From<Prototype> for TypedIdent {
    fn from(v: Prototype) -> Self {
        Self::Function(v)
    }
}

impl TypedIdent {
    pub const fn any(name: ZIdent) -> Self {
        Self::Variable(TypeName {
            name,
            ty: Type::Any,
        })
    }

    pub const fn variable(name: ZIdent, ty: Type) -> Self {
        Self::Variable(TypeName { name, ty })
    }

    pub const fn typevar(name: ZIdent) -> Self {
        Self::TypeVar(TypeName {
            name,
            ty: Type::Var,
        })
    }

    pub fn name_eq(&self, other: &str) -> bool {
        match self {
            TypedIdent::TypeVar(ta) | TypedIdent::Variable(ta) => ta.name.string() == other,
            TypedIdent::Function(p) => p.typename.name.string() == other,
            TypedIdent::Struct(s) => s.typename.name.string() == other,
            TypedIdent::Trait(t) => t.typename.name.string() == other,
        }
    }

    pub fn try_into_type_var(self) -> Result<TypeName, Self> {
        if let Self::TypeVar(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}
impl From<&ZValue> for Type {
    fn from(value: &ZValue) -> Self {
        match value {
            ZValue::Nil => Self::Nil,
            ZValue::Bool(_) => Self::Bool,
            ZValue::Number(_) => Self::Float,
            ZValue::Byte(_) => Self::Byte,
            ZValue::Str(_) => Self::String,
            ZValue::Vec(_) => Self::Vector,
            ZValue::Rune(_) => Self::Rune,
            ZValue::Ident(_) => Self::RuneIdent,
            ZValue::Func(_) => Self::Function,
        }
    }
}
