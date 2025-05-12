#pragma once

#include "common.h"
#include "rune.h"
#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

namespace zeal::ast {

struct SrcBuffer {

  /// Includes name and path relative from root working directory
  /// if loading script from memory, this will (most likely!) be an empty string
  std::string path;
  /// String buffer of the loaded source file
  std::string filebuf{};
  /// vector of string views into the filebuf
  Vec<std::string_view> toks{};

  static SrcBuffer load(const std::string& path);
  static SrcBuffer load(const slice<const byte> mem);
};


} // namespace zeal::ast
