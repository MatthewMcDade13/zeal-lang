#include "incl/ast.h"
#include <cassert>
#include <iostream>

namespace zeal::ast {
void ast() {
  std::cout << "hello from ast!\n";
}

AstNode& AstList::operator[](usize index) {
  assert(index <= this->length);
  auto root = this->list.get();
  auto item = root + index;
  return *item;
}

AstNode& AstArray::operator[](usize index) {
  assert(index <= this->length);
  auto root = this->list.get();
  auto item = root + index;
  return *item;
}

} // namespace zeal::ast
