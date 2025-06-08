#include "incl/ast.h"

#include <cassert>

#include "lex.h"
#include "plog/Log.h"

namespace zeal::ast {

struct Parser {
    cslice<Token> toks;
    usize i{0};
};

IOResult parse_file(const Str filepath) {
    if (const auto res = lex::tokenize_file(filepath); res.has_value()) {
        const auto toks = res.value();
        PLOGD << "Tokenized file: " << filepath << " successfully!";

        return parse(toks);
    } else {
        return ZERR(res.error().to_string());
    }
}

IOResult parse_source(const Str source) {

    if (const auto res = lex::tokenize_input(source); res.has_value()) {
        const auto toks = res.value();
        PLOGD << "Tokenized source input: " << source << " successfully!";

        return parse(toks);
    } else {
        return ZERR(res.error().to_string());
    }
}

IOResult parse(const std::span<const Token> toks) {
    // Parser p{
    //     .toks = toks,
    //     .i = 0,
    // };

    

    return {};
}

}  // namespace zeal::ast
