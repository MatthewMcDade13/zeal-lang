#pragma once

#include <variant>

#include "common.h"
#include "rune.h"
namespace zeal::ast {

namespace expr {

// struct Atom {
// using Unit = std::monostate;
// using Inner = std::variant<Unit, bool, i64, f64, core::Rune>;
// };



enum class OperatorType {
    Add,
    Sub,
    Mul,
    Div,
    Concat,
    Negate,
    Not,
    Modulo,
    Gt,
    Lt,
    Gte,
    Lte,
    Eq,
    NotEq,
    And,
    Or,
    Unknown,
};



/// Index/Slot to another AST Node
struct Slot {
    const u32 slot{0};
    constexpr Slot(u32 s = 0) noexcept : slot(s) {}
};

// Pair Forms
struct Operator {
    const OperatorType type{};
    const Slot args{};
};
struct Assign {
    const Slot lhs{};
    const Slot rhs{};
};

struct Call {
    const Slot head{};
    const Slot args{};
};

using Slots2 = Tup2<Slot, Slot>;
using Slots3 = Tup3<Slot, Slot, Slot>;

using Unit = std::monostate;
using List = Vec<Slot>;
using Symbol = core::Rune;

using PairForm = Union<Operator, Assign, Call, Tup2<Slot, Slot>>;
using TripleForm = Union<Slots3>;

using Atom = Union<Unit, bool, i64, f64, String, Symbol>;
using Expr = Union<Unit, Atom, PairForm, TripleForm, List>;
using ExprStmt = Union<>;


// NOTE: For reference:
/*

#[derive(Debug, Clone)]
pub enum ExprStmt {
    Block(AstList<Self>),
    Loop(AstList<Self>),
    While { cond: Expr, body: AstList<Self> },
    When(AstList<WhenForm>),
    DefFunc(FuncDecl),
    Binding(BindStmt),
    Escape(EscapeExpr),
    Atom(Expr),
}

*/

}  // namespace expr

}  // namespace zeal::ast
