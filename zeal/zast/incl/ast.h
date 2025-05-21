#pragma once

#include "common.h"
#include "rune.h"
#include <stdexcept>
#include <variant>
namespace zeal::ast {
void ast();


using Atom = std::variant<bool, i64, f64, core::Rune>;
using Expr = std::variant<>;
using ExprStmt = std::variant<>;
using Stmt = std::variant<>;


} // namespace zeal::ast
