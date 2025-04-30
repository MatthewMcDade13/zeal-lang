#pragma once


#include <string>
#include "../../typedefs.hpp"

namespace zeal::parse {

  struct Tok {
    struct {
      u32 column;
      u32 row;
    } line{};
    
    struct {
     usize begin; 
     usize end;
    } lexeme{};
  };

  struct TokBuff {

  private:
    std::string source;
    Vec<Tok> tokens;
  };
}
