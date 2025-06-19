#pragma once

#include <cassert>
#include <expected>
#include <variant>

#include "common.h"
#include "plog/Log.h"
namespace zeal::ast {

enum class TokType : i16 {

    Error = -2,
    Unknown = -1,

    /// AST Marker to denote the end of a statement/expression
    Terminal = 0,

    /// single character runes
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
    Eq = '=',

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
    Fn,
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
    /// newtype  :38
    NewType,
    /// continue :39
    Continue,
    /// break    :40
    Break,
    /// return   :41
    Return,
    /// pub      :42
    Pub,
    /// import   :43
    Import,
    /// include  :44
    Include,
    /// const    :45
    Constant,
    /// ==       :46
    DblEq,

    BuiltinEnd,

    LangValuesStart = 0xBEE,

    /// any user string, inculding surrounding '"'
    String,
    /// any symbol prefixed with ':' (even strings ex: :"really long rune but im
    /// still only a rune :)")
    Rune,
    /// any alpha-numeric characters not surrounded by '"' and/or prefixed with ':'
    /// or '@'
    Symbol,
    /// any alpha-numeric characters prefixed with '@'
    Macro,
    Float,
    Integer,

    LangValuesEnd,

};

constexpr const char UNKNOWN_LITERAL[] = "_?_";

// constexpr String toktype_to_string(const TokType tt)  {
//     constexpr const auto glower = static_cast<i16>(TokType::BuiltinStart);
//     constexpr const auto gupper = static_cast<i16>(TokType::BuiltinEnd);

//     constexpr const auto lvlower = static_cast<i16>(TokType::LangValuesStart);
//     constexpr const auto lvupper = static_cast<i16>(TokType::LangValuesEnd);

//     const auto tval = static_cast<i16>(tt);

//     if (tval < glower) {
//         return std::string(1, static_cast<char>(tt));
//     } else if (tval > glower && tval < gupper) {
//         static constexpr std::array<Str, 47> NAMES = {
//             "begin", "end",    "function", "fn",      "do",       "while",  "when",
//             "for",   "if",     "then",     "elseif",  "else",     "struct", "or",
//             "and",   "module", ">=",       "<=",      "+=",       "-=",     "/=",
//             "*=",    "&&",     "||",       "^^",      "..",       "...",    "--",
//             "->",    "=>",     "<-",       "where",   "in",       "let",    "mut",
//             "|>",    "<|",     "**",       "newtype", "continue", "break",  "return",
//             "pub",   "import", "include",  "const",   "=="};
//         const auto i = tval - glower;
//         if (i < NAMES.size()) {
//             return std::string(NAMES[i]);
//         } else {
//             return String(UNKNOWN_LITERAL) + std::to_string(tval);
//         }

//     } else if (tval > lvlower && tval < lvupper) {
//         switch (tt) {
//             case TokType::String: {
//                 return "__String__";
//             } break;
//             case TokType::Rune: {
//                 return "__Rune__";
//             } break;
//             case TokType::Symbol: {
//                 return "__Symbol__";
//             } break;
//             case TokType::Macro: {
//                 return "__Macro__";
//             } break;
//             case TokType::Float: {
//                 return "__Float__";
//             } break;
//             case TokType::Integer: {
//                 return "__Integer__";
//             } break;
//             default: {
//                 return "__TOKTYPE__UNKNOWN__";
//             } break;
//         }
//     } else {
//         return std::string(UNKNOWN_LITERAL) + std::to_string(tval);
//     }

// }

// struct Token {
//     using Empty = std::monostate;
//     using Symbol = core::Rune;
//     using Int = i64;
//     using Float = f64;
//     using Data = std::variant<Empty, Symbol, Int, Float>;

//     u32 lineno{};
//     u32 colno{};

//     TokType type = TokType::Unknown;

//     Data data;

//     // constexpr Token() : lineno(0), colno(0), data(Empty()) {}

//     constexpr std::string_view lexeme_sview() const noexcept {
//         if (std::holds_alternative<Symbol>(this->data)) {
//             const auto& r = std::get<Symbol>(this->data);
//             return r.sview();
//         }
//         return {};
//     }
//     constexpr bool has_lexeme() const noexcept {
//         return std::holds_alternative<Symbol>(this->data);
//     }

//     constexpr bool is_float() const noexcept {
//         return std::holds_alternative<Float>(this->data);
//     }

//     constexpr bool is_integer() const noexcept {
//         return std::holds_alternative<Int>(this->data);
//     }

//     constexpr bool is_int_or_float() const noexcept {
//         return this->is_float() || this->is_integer();
//     }

//     constexpr Opt<u64> try_integer() const noexcept {
//         if (this->is_integer()) {
//             return std::get<Int>(this->data);
//         } else {
//             return std::nullopt;
//         }
//     }

//     constexpr Opt<f64> try_float() const noexcept {
//         if (this->is_integer()) {
//             return std::get<Float>(this->data);
//         } else {
//             return std::nullopt;
//         }
//     }

//     constexpr Opt<Symbol> try_lexeme() const noexcept {
//         if (this->has_lexeme()) {
//             return std::get<Symbol>(this->data);
//         } else {
//             return std::nullopt;
//         }
//     }

//     constexpr String to_string() const {
//         if (const auto rsym = this->try_lexeme()) {
//             const Symbol sym = rsym.value();
//             std::string res(sym);
//             // if rune is empty, try stringifying the type token type instead
//             if (res.size() == 0) {
//                 return toktype_to_string(this->type);
//             }
//             // just to be safe! lol
//             res += "\0";
//             return res;
//         } else if (const auto rint = this->try_integer()) {
//             const auto val = rint.value();
//             return std::to_string(val);

//         } else if (const auto rfloat = this->try_float()) {
//             const auto val = rfloat.value();
//             return std::to_string(val);

//         } else {
//             return {};
//         }
//     }
// };

// namespace lex {

// struct LexError {
//     enum : u8 {
//         Unknown = 0,
//         UnexpectedEof,
//         /// Missing ending '"' or ')', '}', ect
//         MissingCloser,
//         UnknownSymbol,
//         InvalidSymbol,
//         IOFail,
//         /// Any error, read error message to find out what went wrong
//         Any,

//         /// Most likely an exception was thrown when constructing a std::string
//         /// somewhere... hmmm... *sherlock_holms_emoji* lol
//         BadStdStringCtor,
//         LangFeatureNotYetImplemented,
//         Count
//     } errtype;

//     struct Location {
//         usize line{};
//         usize column{};
//     } loc;

//     std::string message;

//     constexpr std::string_view type_string() const noexcept {
//         static constexpr const std::array<std::string_view, Count> NAMES = {
//             "Unknown",          "UnexpectedEof",
//             "MissingCloser",    "UnknownSymbol",
//             "IOFail",           "Any",
//             "BadStdStringCtor", "LangFeatureNotYetImplemented"};
//         const auto ty = static_cast<usize>(this->errtype);
//         try {
//             return NAMES[ty];
//         } catch (...) {
//             PLOGF << "errtype of token calling to_string method is invalid/out of "
//                      "range!!";
//             return {};
//         }
//     }

//     constexpr bool is_terminal(const char c) noexcept {
//        return c == '\n' || c == ';';
//     }
//     constexpr bool isspace_or_terminal(const char c) noexcept {
//         return is_terminal(c) || std::isspace(c);
//     }

//     constexpr String to_string() const {
//         const auto prefix = std::string(this->type_string());
//         return prefix + "::" + this->message;
//     }
// };

// /// LexError's inner error enum type
// using LexError_t = decltype(LexError::errtype);

// /// Tag type representing a successful IO operation that returns no value
// struct LexIOResult {};
// template <typename T = LexIOResult>
// using LexResult = std::expected<T, LexError>;

// LexResult<Vec<Token>> tokenize_file(const Str source_file);
// LexResult<Vec<Token>> tokenize_input(const Str source_memory);
// }  // namespace lex

}  // namespace zeal::ast
