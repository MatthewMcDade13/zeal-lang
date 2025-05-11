#pragma once

#include "common.h"
#include "rune.h"
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

enum class TokType : i16 {
  
  Error = -2,
  Unknown = -1,

  /// AST Marker to denote the end of a statement/expression
  Terminal,

  Plus = '+',
  Minus = '-',
  Star = '*',
  Whack = '/',

  OpenCurly = '{',
  CloseCurly = '}',

  OpenBracket = '[',
  CloseBracket = ']',

  OpenParen = '(',
  CloseParen = ')',

  Band = '!',
  QMark = '?',
  ChevronUp = '^',
  Amp = '&',
  Percent = '%',
  Dollar = '$',
  Hash = '#',
  At = '@',

  SinglePipe = '|',
  DblQuote = '\"',
  Quote = '\'',
  Backtick = '`',
  Tilde = '~',
  Colon = ':',
  SemiColon = ';',
  Lt = '<',
  Gt = '>',

  /// Language keywords (ex: begin, end, struct, ect)
  BuiltinStart = 0xFF,


};

struct Token {
  u32 lineno{};
  u32 colno{};

  TokType type = TokType::Unknown;

  core::Rune lexeme{core::Rune::make_default()};

  constexpr std::string_view as_str() const noexcept {
    return lexeme.sview();

  }

};

} // namespace zeal::ast
