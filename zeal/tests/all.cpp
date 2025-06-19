#include "lex.h"
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"


// TEST_CASE("Test tokenize words") {
//     constexpr const char* SAMPLE = "let x = 12;";
//     const auto ts = zeal::ast::lex::tokenize_input(SAMPLE);

//     // CHECK(ts.has_value());

//     INFO(ts.has_value());
//     if (!ts.has_value()) {
//         const auto err = ts.error();

//         INFO("Error Tokenizing:\n\t=> ", err.message);
//     } else {
//         const auto v = ts.value();
//         CHECK(v.size() >= 1);
//         CHECK(v.size() == 5);
//     }
// }
