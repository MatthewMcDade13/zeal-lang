#include "lex.h"

namespace {
  using namespace zeal;
  using namespace zeal::ast;

struct Lexer {
  struct {
    isize line{};
    isize col{}; 
  } cursor{};

  std::string file{};

  std::vector<Token> result;

};

} // namespace

namespace zeal::ast {

namespace lex {

  ArcVec<Token> tokenize(std::string_view source_file) {
    Lexer lex = {
      .cursor = {},
      .file = std::string(source_file),
      .result = {},
    };
    lex.result.reserve(128);
  }

  ArcVec<Token> tokenize_memory(std::string_view source_memory) {
    
  }
  
}
} // namespace zeal::ast
