#include "lex.h"
#include "rcbuf.h"
#include <fstream>
#include <iostream>
#include <ranges>
#include <sstream>

namespace {
using namespace zeal;
using namespace zeal::ast;

struct Lexer {
  struct {
    isize line{};
    isize col{};
  } cursor{};
};

} // namespace

namespace zeal::ast {

namespace lex {

Opt<core::RcArray<Token>> tokenize(const std::string& filepath) {
  std::ifstream infile(filepath);
  if (!infile) {
    std::cerr << "Could not open file: " << filepath << "\n";
    return std::nullopt;
  }
  std::stringstream ss;
  ss << infile.rdbuf();
  return tokenize_memory(ss.str());
}

Opt<core::RcArray<Token>> tokenize_memory(std::string_view source_memory) {
  Lexer lex = {
      .cursor = {},
  };

  Vec<Token> result;
  result.reserve(source_memory.size());

  auto lines = std::ranges::views::split(source_memory, '\n');

  for (auto&& line_range : lines) {
    lex.cursor.line += 1;
    lex.cursor.col = 0;
    std::string_view line(line_range.begin(), line_range.end());

    if (line.empty()) {
      continue;
    }

    auto words = std::ranges::views::split(line, ' ');

    if (words.empty()) {
      continue;
    }

    for (auto&& word_range : words) {
      const std::string_view word(word_range.begin(), word_range.end());
      lex.cursor.col += word.size();

      const char head = *word.begin();
      switch (head) {
        case '+': {
          break;
        }
        case '-': {
          break;
        }
        case '/': {
          break;
        }
        case '*': {
          break;
        }
        case '%': {
          break;
        }
        case '<': {
            
        }
        case '>': {
            
        }
        case '{': {
            
        }
        case '}': {
            
        }
        case '(': {
            
        }
        case ')': {
            
        }
      }
    }
  }

  // ArcVec<Token>
}

} // namespace lex
} // namespace zeal::ast
