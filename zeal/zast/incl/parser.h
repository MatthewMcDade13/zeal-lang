#pragma once

#include "common.h"
#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

namespace zeal::ast {

struct SrcBuffer {

  /// Includes name and path relative from root working directory
  /// if loading script from memory, this will (most likely!) be an empty string
  std::string path;
  /// String buffer of the loaded source file
  std::string filebuf{};
  /// vector of string views into the filebuf
  Vec<std::string_view> toks{};

  static SrcBuffer load(const std::string& path);
  static SrcBuffer load(const slice<const byte> mem);
};

enum class TokType {
  Unknown = -1,
};

struct Token {
  u32 lineno{};
  u32 colno{};

  TokType type = TokType::Unknown;

  std::variant<std::string, std::string_view> lexeme{std::string("")};

  std::string_view as_str() const noexcept {
    if (const auto* str = std::get_if<std::string>(&this->lexeme)) {
      return std::string_view{*str};
    } else if (const auto* view = std::get_if<std::string_view>(&this->lexeme)) {
      return *view;
    } else {
      return {};
    }
  }

  constexpr bool is_owned() const noexcept {
    return std::holds_alternative<std::string>(this->lexeme);
  }
};

} // namespace zeal::ast
