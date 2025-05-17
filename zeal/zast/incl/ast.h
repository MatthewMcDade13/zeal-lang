#pragma once

#include "common.h"
#include "rune.h"
#include <stdexcept>
#include <variant>
namespace zeal::ast {
void ast();

/// A lisp-like list where the first item is always
/// expected to be a symbol/rune that signifies a function call
struct AstList {
  Arc<struct AstNode[]> list;
  u32 length;

  struct AstNode& operator[](usize index);
};

/// A lisp-like list of pure values. Not to be evaluated
/// for execution
struct AstArray {
  Arc<struct AstNode[]> list;
  u32 length;
  struct AstNode& operator[](usize index);
};

/// Runtime representation of a Zeal abstract syntax tree
/// 1:1 with text form of zeal's lisp AST (zlisp)
/// NOTE: Bytecode is a different represenation of this same AST, so
/// this is entirely different from Zeal Values
struct AstNode {
  enum class Type {
    Uninit,
    Byte,
    Char,
    Int32,
    Int64,
    Float32,
    Float64,
    String,
    Rune,
    List,
    Array,
    OptNode,
    Count
  };

  template<typename T>
  constexpr AstNode(T val): data(val) {}

  constexpr Type type() const noexcept {
    constexpr usize COUNT = static_cast<usize>(Type::Count);
    constexpr Type TYPE_INDEX[COUNT] = {Type::Uninit, Type::Byte, Type::Char, Type::Int32, Type::Int64, Type::Float32, Type::Float64, Type::String, Type::Rune, Type::List, Type::Array, Type::OptNode};

    return TYPE_INDEX[this->data.index()];
  }

  constexpr usize type_index() const noexcept {
    return this->data.index();
  }
  
  template<typename T>
  Opt<T> try_get() const noexcept {
    if (std::holds_alternative<T>(&this->data)) {
      return std::get<T>(&this->data);
    }
    return std::nullopt;
  }

  template<typename T>
  bool try_set(T&& val) {
    data.emplace(std::move(val));  
  }


private:
  using OptNode = std::optional<Arc<AstNode>>;

  std::variant<std::monostate, byte, char, i32, i64, f32, f64, core::BoxStr, core::Rune,
               AstList, AstArray, OptNode>
      data;
};
} // namespace zeal::ast
