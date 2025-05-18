#pragma once

#include <cassert>
#include <expected>

#include "common.h"
#include "rcbuf.h"
#include "rune.h"
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

  /// any user string, inculding surrounding '"'
  String,
  /// any symbol prefixed with ':' (even strings ex: :"really long rune but im still only a rune :)")
  Rune,
  /// any alpha-numeric characters not surrounded by '"' and/or prefixed with ':' or '@'
  Symbol,
  /// any alpha-numeric characters prefixed with '@'
  Macro,

};

/// Namespace for builting reserved language rune literals
/// such as begin, end, function, ect...
struct RuneKeywords {
  static constexpr const auto Count = static_cast<i16>(TokType::BuiltinEnd) - static_cast<i16>(TokType::BuiltinStart);

  static constexpr std::array<std::string_view, Count> Names = {
      "begin", "end", "function", "fn", "do", "while", "when", "for", "if",  "then", "elseif", "else", "struct",
      "or",    "and", "module",   ">=", "<=", "+=",    "-=",   "/=",  "*=",  "&&",   "||",     "^^",   "..",
      "...",   "--",  "->",       "=>", "<-", "where", "in",   "let", "mut", "|>",   "<|",     "**"};

  template <TokType type>
  static constexpr std::string_view lookup() noexcept {
    constexpr const i16 lower_limit = static_cast<i16>(TokType::BuiltinStart);
    constexpr const i16 upper_limit = static_cast<i16>(TokType::BuiltinEnd);
    constexpr const i16 id = static_cast<i16>(type);
    static_assert(id > lower_limit && id < upper_limit, "Invalid tok type to lookup RuneKeyword!");

    constexpr const isize index = upper_limit - id;
    static_assert(index >= 0 && index <= Count,
                  "Index our of range when looking up string representation of RuneKeyword!");

    return Names[index];
  }

  static constexpr const std::string_view Begin() noexcept { return lookup<TokType::Begin>(); }

  static constexpr const std::string_view End() noexcept { return lookup<TokType::End>(); }
};

struct Token {
  u32 lineno{};
  u32 colno{};

  TokType type = TokType::Unknown;

  core::Rune lexeme{core::Rune::make_default()};

  constexpr std::string_view lexeme_string() const noexcept { return lexeme.sview(); }
  constexpr bool has_lexeme() const noexcept { return lexeme.sview().size() >= 1; }
};

namespace lex {

struct LexError {
  enum : u8 {
    UnexpectedEof,
    /// Missing ending '"' or ')', '}', ect
    MissingCloser,
    UnknownSymbol,
    InvalidSymbol,
    IOFail,
    /// Any error, read error message to find out what went wrong
    Any,

    /// Most likely an exception was thrown when constructing a std::string somewhere... hmmm... *sherlock_holms_emoji* lol
    BadStdStringCtor,
    LangFeatureNotYetImplemented,
  } errtype;

  struct Location {
    usize line{};
    usize column{};
  } loc;

  std::string message;
};

/// LexError's inner error enum type
using LexError_t = decltype(LexError::errtype);


/// Tag type representing a successful IO operation that returns no value
struct IOResult {};
template <typename T = IOResult>
using LexResult = std::expected<T, LexError>;

LexResult<core::RcArray<Token>> tokenize(const std::string& source_file);
LexResult<core::RcArray<Token>> tokenize_memory(const std::string_view source_memory);
}  // namespace lex

}  // namespace zeal::ast
