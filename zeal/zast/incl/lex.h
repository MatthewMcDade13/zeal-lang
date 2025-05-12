#pragma once



#include "common.h"
#include "rune.h"
namespace zeal::ast {

enum class TokType : i16 {
  
  Error = -2,
  Unknown = -1,

  /// AST Marker to denote the end of a statement/expression
  Terminal,

  /// Single character Runes
  Plus = '+',
  Minus = '-',
  Star = '*',
  Whack = '/',

  Bang = '!',
  QMark = '?',
  ChevronUp = '^',
  Amp = '&',
  Percent = '%',
  Dollar = '$',
  At = '@',

  SinglePipe = '|',
  OpenCurly = '{',
  CloseCurly = '}',

  OpenBracket = '[',
  CloseBracket = ']',

  OpenParen = '(',
  CloseParen = ')',


  DblQuote = '\"',
  Quote = '\'',
  Backtick = '`',
  Tilde = '~',
  Colon = ':',
  SemiColon = ';',
  Lt = '<',
  Gt = '>',
  Dot = '.',

  /// Language keywords (ex: begin, end, struct, ect)
  /// and runes more than 1 character long
  BuiltinStart = 0xFF,
  /// begin
  Begin,
  /// end
  End,
  /// function
  Function,
  /// fn
  Lambda,
  /// do
  Do,
  While,
  For,
  If,
  Then,
  Elseif,
  Else,
  Struct,
  Module,
  /// >=
  Gte,
  /// <=
  Lte,
  /// +=
  PlusEq,
  /// -=
  MinusEq,
  /// /=
  DivEq,
  /// *=
  MulEq,


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
  

}

