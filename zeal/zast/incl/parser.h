#pragma once

#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace zeal::ast {

struct SourceFile {

  /// Includes name and path from root working directory
  std::string path;
  /// String buffer of the loaded source file
  std::string filebuf{};
  /// vector of string views into the filebuf
  std::vector<std::string_view> toks{};

  static SourceFile load(const std::string& path);
  static SourceFile load(const std::span<const std::byte> mem);
};



} // namespace zeal::ast
