#pragma once

#include <cassert>
#include <expected>
#include <variant>

#include "common.h"
#include "plog/Log.h"
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

    Float,
    Integer,

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
    NewType,
    Continue,
    Break,
    Return,
    Pub,
    Import,
    Include,
    Constant,

    BuiltinEnd,

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

};

struct Token {
    using Empty = std::monostate;
    using Symbol = core::Rune;
    using Int = i64;
    using Float = f64;
    using Data = std::variant<Empty, Symbol, Int, Float>;

    u32 lineno{};
    u32 colno{};

    TokType type = TokType::Unknown;

    Data data;

    // constexpr Token() : lineno(0), colno(0), data(Empty()) {}

    constexpr std::string_view lexeme_sview() const noexcept {
        if (std::holds_alternative<Symbol>(this->data)) {
            const auto& r = std::get<Symbol>(this->data);
            return r.sview();
        }
        return {};
    }
    constexpr bool has_lexeme() const noexcept {
        return std::holds_alternative<core::Rune>(this->data);
    }

    constexpr bool is_float() const noexcept {
        return std::holds_alternative<Float>(this->data);
    }

    constexpr bool is_integer() const noexcept {
        return std::holds_alternative<Int>(this->data);
    }

    constexpr bool is_int_or_float() const noexcept {
        return this->is_float() || this->is_integer();
    }
};

namespace lex {

struct LexError {
    enum : u8 {
        Unknown = 0,
        UnexpectedEof,
        /// Missing ending '"' or ')', '}', ect
        MissingCloser,
        UnknownSymbol,
        InvalidSymbol,
        IOFail,
        /// Any error, read error message to find out what went wrong
        Any,

        /// Most likely an exception was thrown when constructing a std::string
        /// somewhere... hmmm... *sherlock_holms_emoji* lol
        BadStdStringCtor,
        LangFeatureNotYetImplemented,
        Count
    } errtype;

    struct Location {
        usize line{};
        usize column{};
    } loc;

    std::string message;

    constexpr std::string_view to_string() const noexcept {
        static constexpr const std::array<std::string_view, Count> NAMES = {
            "Unknown",          "UnexpectedEof",
            "MissingCloser",    "UnknownSymbol",
            "IOFail",           "Any",
            "BadStdStringCtor", "LanFeatureNotYetImplemented"};
        const auto ty = static_cast<usize>(this->errtype);
        try {
            return NAMES[ty];
        } catch (...) {
            PLOGF << "errtype of token calling to_string method is invalid/out of "
                     "range!!";
            return {};
        }
    }
};

/// LexError's inner error enum type
using LexError_t = decltype(LexError::errtype);

/// Tag type representing a successful IO operation that returns no value
struct IOResult {};
template <typename T = IOResult>
using LexResult = std::expected<T, LexError>;

LexResult<Vec<Token>> tokenize(const std::string& source_file);
LexResult<Vec<Token>> tokenize_memory(const std::string_view source_memory);
}  // namespace lex

}  // namespace zeal::ast
