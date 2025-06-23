#ifndef _ZEAL_AST_INCL_LEXER_H_
#define _ZEAL_AST_INCL_LEXER_H_
#include "common.h"

ZEAL_CAPI_BEGIN

enum zl_TokenType {

    zl_TokType__Error = -2,
    zl_TokType__Unknown = -1,

    /// AST Marker to denote the end of a statement/expression
    zl_TokType__Terminal = 0,

    /// single character runes
    zl_TokType__Plus = '+',
    zl_TokType__Minus = '-',
    zl_TokType__Star = '*',
    zl_TokType__Whack = '/',

    zl_TokType__Bang = '!',
    zl_TokType__QMark = '?',
    zl_TokType__ChevronUp = '^',
    zl_TokType__Amp = '&',
    zl_TokType__Percent = '%',
    zl_TokType__Dollar = '$',
    zl_TokType__At = '@',
    zl_TokType__Eq = '=',

    zl_TokType__SinglePipe = '|',
    zl_TokType__OpenCurly = '{',
    zl_TokType__CloseCurly = '}',

    zl_TokType__OpenBracket = '[',
    zl_TokType__CloseBracket = ']',

    zl_TokType__OpenParen = '(',
    zl_TokType__CloseParen = ')',

    zl_TokType__DblQuote = '\"',
    zl_TokType__Quote = '\'',
    zl_TokType__Backtick = '`',
    zl_TokType__Tilde = '~',
    zl_TokType__Colon = ':',
    zl_TokType__SemiColon = ';',
    zl_TokType__Lt = '<',
    zl_TokType__Gt = '>',
    zl_TokType__Dot = '.',

    /// Language keywords (ex: begin, end, struct, ect)
    /// and runes more than 1 character long
    zl_TokType__BuiltinStart = 0xFF,
    /// begin    :0
    zl_TokType__Begin,
    /// end      :1
    zl_TokType__End,
    /// function :2
    zl_TokType__Function,
    /// fn       :3
    zl_TokType__Fn,
    /// do       :4
    zl_TokType__Do,
    /// while    :5
    zl_TokType__While,
    /// when     :6
    zl_TokType__When,
    /// for      :7
    zl_TokType__For,
    /// if       :8
    zl_TokType__If,
    /// then     :9
    zl_TokType__Then,
    /// elseif   :10
    zl_TokType__Elseif,
    /// else     :11
    zl_TokType__Else,
    /// struct   :12
    zl_TokType__Struct,
    /// or       :13
    zl_TokType__Or,
    /// and      :14
    zl_TokType__And,
    /// mod      :15
    zl_TokType__Module,
    /// >=       :16
    zl_TokType__Gte,
    /// <=       :17
    zl_TokType__Lte,
    /// +=       :18
    zl_TokType__PlusEq,
    /// -=       :19
    zl_TokType__MinusEq,
    /// /=       :20
    zl_TokType__DivEq,
    /// *=       :21
    zl_TokType__MulEq,

    /// &&       :22
    zl_TokType__DblAmp,
    /// ||       :23
    zl_TokType__DlbPipe,
    /// ^^       :24
    zl_TokType__DblChevronUp,
    /// ..       :25
    zl_TokType__DblDot,
    /// ...      :26
    zl_TokType__TripleDot,
    /// --       :27
    zl_TokType__ArrowRight,
    /// =>       :29
    zl_TokType__FatArrowRight,
    /// <-       :30
    zl_TokType__ArrowLeft,
    /// where    :31
    zl_TokType__Where,
    /// in       :32
    zl_TokType__In,
    /// let      :33
    zl_TokType__Let,
    /// mut      :34
    zl_TokType__Mut,
    /// |>       :35
    zl_TokType__PipeRight,
    /// <|       :36
    zl_TokType__PipeLeft,
    /// **       :37
    zl_TokType__DblStar,
    /// newtype  :38
    zl_TokType__NewType,
    /// continue :39
    zl_TokType__Continue,
    /// break    :40
    zl_TokType__Break,
    /// return   :41
    zl_TokType__Return,
    /// pub      :42
    zl_TokType__Pub,
    /// import   :43
    zl_TokType__Import,
    /// include  :44
    zl_TokType__Include,
    /// const    :45
    zl_TokType__Constant,
    /// ==       :46
    zl_TokType__DblEq,

    zl_TokType__BuiltinEnd,

    zl_TokType__LangValuesStart = 0xBEE,

    /// any user string, inculding surrounding '"'
    zl_TokType__String,
    /// any symbol prefixed with ':' (even strings ex: :"really long rune but im
    /// still only a rune :)")
    Rzl_TokType__une,
    /// any alpha-numeric characters not surrounded by '"' and/or prefixed with ':'
    /// or '@'
    zl_TokType__Symbol,
    /// any alpha-numeric characters prefixed with '@'
    zl_TokType__Macro,
    zl_TokType__Float,
    zl_TokType__Integer,

    zl_TokType__LangValuesEnd,
};

/// An iterator to current token
typedef struct {
    /// Pointer slice to mmapped memory
    zl_StrSlice memory;
    /// Pointer slice to current Token
    zl_StrSlice token;

    union {
        f64 floatp;
        i64 integer;
        zl_StrSlice string;
    };

    /// Line number where this token is at in file
    i32 lineno;
    /// Column (line index) number of this token
    i32 colno;

    /// Type of Zeal Language Source File Token
    zl_TokenType type;

} zl_TokStream;

ZEAL_CAPI_END

#endif
// #include <cassert>
// #include <expected>
// #include <variant>
//
// #include "common.h"
// #include "plog/Log.h"
// namespace zeal::ast {

// constexpr const char UNKNOWN_LITERAL[] = "_?_";

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
//             "begin", "end",    "function", "fn",      "do",       "while", "when",
//             "for",   "if",     "then",     "elseif",  "else",     "struct", "or",
//             "and",   "module", ">=",       "<=",      "+=",       "-=",     "/=",
//             "*=",    "&&",     "||",       "^^",      "..",       "...",    "--",
//             "->",    "=>",     "<-",       "where",   "in",       "let",    "mut",
//             "|>",    "<|",     "**",       "newtype", "continue", "break",
//             "return", "pub",   "import", "include",  "const",   "=="};
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
//             PLOGF << "errtype of token calling to_string method is invalid/out of
//             "
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

// }  // namespace zeal::ast
