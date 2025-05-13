#pragma once

#include "common.h"
#include "rune.h"
#include <cassert>
namespace zeal::ast {

enum class TokType : i16 {

  Error = -2,
  Unknown = -1,

  /// AST Marker to denote the end of a statement/expression
  Terminal = 0,

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
  /// begin    :0
  Begin,
  /// end      :1
  End,
  /// function :2
  Function,
  /// fn       :3
  Lambda,
  /// do       :4
  Do,
  /// while    :5
  While,
  /// when     :6
  When,
  /// for      :7
  For,
  /// if       :8
  If,
  /// then     :9
  Then,
  /// elseif   :10
  Elseif,
  /// else     :11
  Else,
  /// struct   :12
  Struct,
  /// or       :13
  Or,
  /// and      :14
  And,
  /// mod      :15
  Module,
  /// >=       :16
  Gte,
  /// <=       :17
  Lte,
  /// +=       :18
  PlusEq,
  /// -=       :19
  MinusEq,
  /// /=       :20
  DivEq,
  /// *=       :21
  MulEq,

  /// &&       :22
  DblAmp,
  /// ||       :23
  DlbPipe,
  /// ^^       :24
  DblChevronUp,
  /// ..       :25
  DblDot,
  /// ...      :26
  TripleDot,
  /// --       :27
  DblMinus,
  /// ->       :28
  ArrowRight,
  /// =>       :29
  FatArrowRight,
  /// <-       :30
  ArrowLeft,
  /// where    :31
  Where,
  /// in       :32
  In,
  /// let      :33
  Let,
  /// mut      :34
  Mut,
  /// |>       :35
  PipeRight,
  /// <|       :36
  PipeLeft,
  /// **       :37
  DblStar,
  BuiltinEnd,

};

/// Namespace for builting reserved language rune literals
/// such as begin, end, function, ect...
struct RuneLiterals {
  static constexpr const auto Count = static_cast<i16>(TokType::BuiltinEnd) -
                                      static_cast<i16>(TokType::BuiltinStart);

  static constexpr const char* Names[Count] = {
      "begin", "end",  "function", "fn",   "do",     "while", "when", "for",
      "if",    "then", "elseif",   "else", "struct", "or",    "and",  "module",
      ">=",    "<=",   "+=",       "-=",   "/=",     "*=",    "&&",   "||",
      "^^",    "..",   "...",      "--",   "->",     "=>",    "<-",   "where",
      "in",    "let",  "mut",      "|>",   "<|",     "**"
  };

  static constexpr const char* lookup(TokType type) {
    constexpr const i16 lower_limit = static_cast<i16>(TokType::BuiltinStart);
    constexpr const i16 upper_limit = static_cast<i16>(TokType::BuiltinEnd);
    const i16 id = static_cast<i16>(type);

    if (id <= lower_limit || id >= upper_limit) {
      return ""; 
    }
    const isize index = lower_limit - id;
    assert(index >= 0 && index <= Count);

    return Names[index]; 
    
  }
};


struct Token {
  u32 lineno{};
  u32 colno{};

  TokType type = TokType::Unknown;

  core::Rune lexeme{core::Rune::make_default()};

  constexpr std::string_view as_str() const noexcept { return lexeme.sview(); }
};


namespace lex {  

  // struct LexError {
  //   enum : i8 {
  //     UnknownSymbol,
  //     InvalidSymbol,
  //   };

  //   isize at_line{};
  //   isize at_column{};

  // };

  // using LexResult = std::variant<std::monostate, ArcVec<Token>, ArcVec<LexError>>; 
  
  ArcVec<Token> tokenize(std::string_view source_file);
  ArcVec<Token> tokenize_memory(std::string_view source_memory);
}


} // namespace zeal::ast
