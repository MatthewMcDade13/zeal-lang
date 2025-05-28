#pragma once

#include <sfl/small_unordered_flat_set.hpp>
#include <variant>

#include "common.h"
#include "rune.h"

namespace zeal::ast {

/// A collection of unique strings (separated by a single whitespace in memory)
/// a small unordered flat set is used to track strings that are already in table.
/// Runes are distinguised from symbols by their ':' prefix
/// NOTE: currently all symbols are promoted to Runes at some point, so their names will most likely
/// be used interchangibly. (Runes are just internned strings, and since symbols are pretty much static Syntax data,
/// they are an easy target for string interning). Just keep in mind that :symbol != symbol
///
/// @warning std::string_view 's returned from lookup and insertion are only valid the lifetime of this Table.
/// and for that reason this table is not copyable
struct SymbolTable final {

    constexpr SymbolTable() noexcept: seen(), symbols() {}

    Str add(const Str symbol);
    Opt<Str> lookup(const Str symbol) const;
    
   private:
       SymbolTable(const SymbolTable&) = delete;
       SymbolTable& operator=(const SymbolTable&) = delete;

       sfl::small_unordered_flat_set<Str,  64> seen{};
       String symbols{};
};

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
    // enum class Type {
    // Atom,
    // Expr,
    // ExprStmt,
    // };
    // const Type type{Type::Atom};
    const u32 slot{0};
    constexpr Slot(const u32 s = 0) noexcept : slot(s) {}
    // constexpr Slot(u32 s = 0, Type ty = Type::Atom) noexcept
    //     : type(ty), slot(s) {}
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

struct BasicBlock {
    Vec<Slot> statements;
};

/// Same as @ref [BasicBlock], but loops back to beginning of block after executing
/// last statement, forever. (lke rust's loop {} block)
struct LoopBlock {
    Vec<Slot> statements;
};

struct WhileBlock {
    Slot condition;
    Vec<Slot> body;
};

struct DefFunc {};

struct Binding {};

struct EscapeExpr {};

using Slots2 = Tup2<Slot, Slot>;
using Slots3 = Tup3<Slot, Slot, Slot>;

using Unit = std::monostate;
using List = Vec<Slot>;
using Symbol = core::Rune;

using PairForm = Union<Operator, Assign, Call, Tup2<Slot, Slot>>;
using TripleForm = Union<Slots3>;

using Atom = Union<Unit, bool, i64, f64, String, Symbol>;
using Expr = Union<Unit, Atom, PairForm, TripleForm, List>;
using ExprStmt = Union<Expr, BasicBlock, LoopBlock>;

struct Ast {};

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
