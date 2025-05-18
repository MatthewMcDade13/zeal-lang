#include "lex.h"

#include <algorithm>
#include <cctype>
#include <fstream>
#include <iostream>
#include <ranges>
#include <sstream>

#include "rcbuf.h"

namespace {
using namespace zeal;
using namespace zeal::ast;
using namespace zeal::core;

struct Lexer {
    struct {
        isize line{};
        isize col{};
    } cursor{};
    isize pos;
    std::string_view sv;

    /// Create a token of template type [TokType] at current line and column
    /// location
    template <TokType type>
    constexpr Token make_token(std::string_view lexeme = "") const noexcept {
        return Token{
            .lineno = static_cast<u32>(this->cursor.line),
            .colno = static_cast<u32>(this->cursor.col),
            .type = type,
            .lexeme = Rune::from(lexeme),
        };
    }

    constexpr char peek(isize n = 0) const noexcept {
        const isize len = this->sv.size();
        if (this->pos >= len || this->pos < 0) {
            return -1;
        }
        return this->sv[this->pos + n];
    }
    /// Convience function for this->peek(1);
    constexpr char peek_next() const noexcept { return this->peek(1); }

    /// Convience function for this->peek(-1);
    constexpr char peek_prev() const noexcept { return this->peek(-1); }

    constexpr lex::LexError make_error(
        const lex::LexError_t errty,
        const std::string_view message = "") const noexcept {
        const lex::LexError::Location loc{
            .line = static_cast<usize>(this->cursor.line),
            .column = static_cast<usize>(this->cursor.col),
        };
        // NOTE: I hate try-catch, but alas this is Sepples... smh lol
        // ill do whatever i can to prevent exception if its reasonable
        // THOUGHTS(mmcdade): Exceptions are good for situations where you would
        // panic in rust or go, but otherwise id rather never use them as errors as
        // values like std::expected<T, E> is the right way to go about errors,
        // especially for projects like Zeal where there are lots of errors, yes, but
        // that doesn't mean the program is in an invalid state, (lex and parse
        // errors in a repl for example) so we don't want to panic and bail out in
        // most situations id say.
        try {
            // we don't want this to throw... thanks sepples!!!!
            const std::string m = std::string(message);

            return lex::LexError{
                .errtype = errty,
                .loc = loc,
                .message = m,
            };
        } catch (const std::exception& e) {
            // NOTE: if we got here, std::string threw an exception... ugh...
            // THOUGHTS(mmcdade): i can't wait to make a simpler, better systems
            // language than Rust, C++ or Zig...
            return lex::LexError{.errtype = lex::LexError_t::BadStdStringCtor,
                                 .loc = loc,
                                 .message{}};
        }
    }

    constexpr lex::LexError make_error(
        const u8 errty, const std::string_view message = "") const noexcept {
        /// NOTE: the following assignment is... gross... icky yuck-yuck!
        /// *vomit_emoji* but also kinda neato, i forgot about decltype! Keeping this
        /// for future reference/reminder
        const auto type = static_cast<decltype(lex::LexError::errtype)>(errty);
        return this->make_error(type, message);
    }

    /// advances pos and cursor as long as current character under
    /// this->pos is equal to @param c
    /// @returns number of characters that were skipped until this->peek() !=
    /// @param c OR we have reached the end of file/source
    usize adv_while(const char c) noexcept {
        usize count = 0;
        while (!this->is_eos() && this->peek() == c) {
            this->adv();
            count++;
        }
        return count;
    }

    /// Same as @see [adv_while] but advances while the
    /// peeked token is NOT equal to @param c
    usize adv_while_not(const char c) noexcept {
        usize count = 0;
        while (!this->is_eos() && this->peek() != c) {
            this->adv();
            count++;
        }
        return count;
    }

    /// Advances this->pos until this->peek() is equal to @param c
    /// OR this->is_eof() == true.
    /// @returns number of characters skipped, or a value < 0 if we
    /// got to end of input before seeing the desired character
    template <typename Callback>
    isize adv_while_and(const char c, Callback cb) noexcept {
        isize count{0};
        while (!this->is_eos() && this->peek() != c) {
            if (this->peek() == '\n') {
                this->newline_adv();
            } else {
                this->adv();
            }
            count++;
            const auto old_pos = this->pos;

            cb(this);

            const auto dt = this->pos - old_pos;
            // user-given callback did not we didnt advance at all
            if (dt == 0) {
            }
        }

        if (this->peek() == c) {
            return count;
        }

        return -1;
    }

    // template<typename Callback>
    // usize adv_do_while() noexcept {

    // }

    /// Advances this->pos by @param n and increments cursor column by 1
    void adv(isize n = 1) noexcept {
        this->pos += n;
        this->cursor.col++;
    }
    /// Same as @see [adv] but increments cursor line by one and
    /// resets cursor column to 0
    void newline_adv(isize n = 1) noexcept {
        this->pos += n;
        this->cursor.line += 1;
        this->cursor.col = 0;
    }

    /// Is end of source?
    constexpr bool is_eos() const noexcept {
        if (this->pos >= 0 && static_cast<usize>(this->pos) < this->sv.size()) {
            return false;
        } else {
            return true;
        }
    }
};

}  // namespace

namespace zeal::ast {

namespace lex {

LexResult<core::RcArray<Token>> tokenize(const std::string& filepath) {
    std::ifstream infile(filepath);

    std::stringstream ss;
    if (!infile) {
        ss << "Could not open file: " << filepath << "\n";
        const LexError err{.errtype = LexError::IOFail, .loc{}, .message = ss.str()};
        return std::unexpected(err);
    }
    ss << infile.rdbuf();
    return tokenize_memory(ss.str());
}

#define push_token(OUT, TY)                              \
    do {                                                 \
        OUT.emplace_back(lex.make_token<TokType::TY>()); \
    } while (false);

#define push_lexeme(OUT, TY, LEXEME)                           \
    do {                                                       \
        OUT.emplace_back(lex.make_token<TokType::TY>(LEXEME)); \
    } while (false);

LexResult<> tokenize_alnum(Lexer& lex, Vec<Token>& out_buffer) {}

/// tokenizes non-alphanumeric symbols like '+', '=>', ect
/// pushes tokens into @param tok_buffer
LexResult<> tokenize_glyphs(Lexer& lex, Vec<Token>& out_buffer) {
    switch (lex.peek()) {
        case '+': {
            lex.adv();
            if (lex.peek(1) == '=') {
                lex.adv();
                push_token(out_buffer, PlusEq);
            } else {
                push_token(out_buffer, Plus);
            }
            return {};
        } break;
        case '-': {
            lex.adv();
            if (lex.peek(1) == '>') {
                lex.adv();
                push_token(out_buffer, ArrowRight);
            } else if (lex.peek(1) == '=') {
                lex.adv();
                push_token(out_buffer, MinusEq);
            } else {
                push_token(out_buffer, Minus);
            }
            return {};
        } break;
        case '/': {
            lex.adv();
            if (lex.peek_next() == '=') {
                lex.adv();
                push_token(out_buffer, DivEq);
            } else {
                push_token(out_buffer, Whack);
            }
            return {};
        } break;
        case '*': {
            lex.adv();
            if (lex.peek_next() == '=') {
                lex.adv();
                push_token(out_buffer, MulEq);
            } else {
                push_token(out_buffer, Star);
            }
            return {};
        } break;
        case '%': {
            lex.adv();
            push_token(out_buffer, Percent);
            return {};
        } break;
        case '<': {
            lex.adv();
            if (lex.peek_next() == '-') {
                lex.adv();
                push_token(out_buffer, ArrowLeft);

            } else if (lex.peek_next() == '=') {
                lex.adv();
                push_token(out_buffer, Lte);
            } else {
                push_token(out_buffer, Lt);
            }
            return {};
        } break;
        case '>': {
            lex.adv();
            if (lex.peek_next() == '=') {
                lex.adv();
                push_token(out_buffer, Gte);
            } else {
                push_token(out_buffer, Gt);
            }
            return {};
        } break;
        case '{': {
            lex.adv();
            push_token(out_buffer, OpenCurly);
            return {};
        } break;
        case '}': {
            lex.adv();
            push_token(out_buffer, CloseCurly);
            return {};
        } break;
        case '(': {
            lex.adv();
            push_token(out_buffer, OpenParen);
            return {};
        } break;
        case ')': {
            lex.adv();
            push_token(out_buffer, CloseParen);
            return {};
        } break;
        case '"': {
            const auto begin = lex.pos;
            lex.adv();
            while (!lex.is_eos() && lex.peek() != '"') {
                // allow escaped inner double quotes
                if (lex.peek() == '\\' && lex.peek(1) == '"') {
                    lex.adv(2);
                }
                if (lex.peek() == '\n') {
                    // TODO: Support multi-line strings
                    std::cerr << "multiline strings not yet supported!\n";
                    return std::unexpected(
                        lex.make_error(LexError_t::LangFeatureNotYetImplemented,
                                       "Multiline strings not yet supported!"));
                }
            }
            // double check we havent gotten to end of input
            if (lex.peek() == '"') {
                const auto end = lex.pos;
                push_lexeme(out_buffer, String, lex.sv.substr(begin, end));
                lex.adv();
                return {};
            } else {
                std::cerr << "Mismatched ending '\"' found on: line: "
                          << lex.cursor.line << " column: " << lex.cursor.col
                          << "\n";
                return {};
            }

        } break;
        case '!': {
            lex.adv();
            push_token(out_buffer, Bang);
            return {};
        } break;
        case '?': {
            lex.adv();
            push_token(out_buffer, QMark);
            return {};
        } break;
        default: {
            using std::operator""s;
            return std::unexpected(LexError{
                .errtype = LexError::Any,
                .loc =
                    {
                        .line = static_cast<usize>(lex.cursor.line),
                        .column = static_cast<usize>(lex.cursor.col),
                    },
                .message =
                    "Could not determine glyph: "s + std::to_string(lex.peek()),
            });
        }
    }
}

LexResult<core::RcArray<Token>> tokenize_memory(
    const std::string_view source_memory) {
    Lexer lex = {.cursor = {}, .pos = 0, .sv = source_memory};

    Vec<Token> result;
    result.reserve(lex.sv.size());

    const auto len = static_cast<isize>(lex.sv.size() - 1);

    while (!lex.is_eos() <= len) {
        const auto i = lex.pos;
        const auto curr = lex.sv[i];

        if (curr == '\n') {
            lex.newline_adv();
        }

        // advance through whitespace
        if (std::isspace(curr)) {
            lex.adv();
            continue;
        }

        const auto next = i + 1 >= len ? curr : lex.sv[i + 1];

        if (std::isalnum(curr)) {
            if (const auto res = tokenize_alnum(lex, result); !res.has_value()) {
                return std::unexpected(res.error());
            }

            continue;
        } else {
            if (const auto res = tokenize_glyphs(lex, result); !res.has_value()) {
                return std::unexpected(res.error());
            }

            continue;
        }
    }
    return result;
}

}  // namespace lex

}  // namespace zeal::ast
