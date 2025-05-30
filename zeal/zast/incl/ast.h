#pragma once

#include <plog/Log.h>

#include <sfl/small_unordered_flat_map.hpp>
#include <variant>

#include "common.h"
#include "rune.h"
#include "string_table.h"

namespace zeal::ast {

namespace expr {


enum class OperatorType {

    Unknown,
    /// +
    Add,
    /// -
    Sub,
    /// *
    Mul,
    /// /
    Div,
    /// ++
    Concat,
    /// ! (or maybe '--'?)
    Negate,
    /// not
    Not,
    /// % (or maybe 'mod')
    Modulo,
    /// >
    Gt,
    /// <
    Lt,
    /// >=
    Gte,
    /// <=
    Lte,
    /// ==
    Eq,
    /// !=
    NotEq,
    /// and (or '&&')
    And,
    /// or (or '||')
    Or,
    /// $ (or call/invoke, maybe something else)
    Call,
    /// |>
    PipeRight,
    /// => (or 'mkv')
    MapKV,

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
    /// Must be a list of at least 2 exprs
    const Slot args{};
};

struct Assign {
    const Slot lhs{};
    const Slot rhs{};
};

struct Call {
    const Slot head{};
    /// Must be a list
    const Slot args{};
};

struct BasicBlock {
    /// Must be a list
    const Slot statements;
};

/// Same as @ref [BasicBlock], but loops back to beginning of block after executing
/// last statement, forever. (lke rust's loop {} block)
struct LoopBlock {
    /// must be a list of exprs
    const Slot body;
};

struct WhileBlock {
    const Slot condition;
    /// Must be a list of Exprs
    const Slot body;
};

struct DefFunc {
    const core::StringTable::Handle name;
    /// Must be a list of bindings
    const Slot params;
    const Slot body;
};


struct Binding {
    enum class Type : u8 {
        Let,
        Mut,
        Const,
        Param,
        Field,
        MapPair,
    };

    Type type;


    /// Slot to initilizer expr node. 0 if none
    /// Binding types Let and Const MUST have a non 0 initializer
    Slot initializer{0};

    core::StringTable::Handle name;   

    // TODO: Add typeinfo
};

struct EscapeStmt {
    enum class Type : u8 {
        Return,
        Break,
        Continue,
        Try,
        Catch,
        Expect,
    };
    Type type;

    /// If type is Return, must but non-0 slot!
    Slot expr{0};
};


using Unit = std::monostate;
using List = Vec<Slot>;
using Symbol = core::Rune;

using PairForm = Union<Operator, Assign, Call, Tup2<Slot, Slot>>;
using TripleForm = Union<DefFunc, Binding, Tup3<Slot, Slot, Slot>>;

using Atom = Union<Unit, bool, i64, f64, core::StringTable::Handle>;
using Expr = Union<Unit, Atom, PairForm, TripleForm, List>;
using ExprStmt = Union<Expr, BasicBlock, LoopBlock, EscapeStmt>;


}  // namespace expr

// struct Ast {
//     Vec<expr::ExprStmt> root; 
// };

IOResult parse_file(const Str filepath);
IOResult parse_source(const Str source);
IOResult parse(const std::span<const struct Token> toks);


}  // namespace zeal::ast
