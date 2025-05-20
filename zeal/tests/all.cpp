#include "lex.h"
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

int factorial(int number) {
    return number <= 1 ? number : factorial(number - 1) * number;
}

TEST_CASE("Test tokenize words") {
    constexpr const char* SAMPLE = "let x = 12;";
    const auto ts = zeal::ast::lex::tokenize_memory(SAMPLE);
    if (!ts.has_value()) {
        const auto err = ts.error();


        INFO("Error Tokenizing:\n\t=> ", err.message);
        CHECK(ts.has_value());
    } else {
        const auto v = ts.value();

        CHECK(v.size() >= 1);
        CHECK(v.size() == 5);
    }
}
