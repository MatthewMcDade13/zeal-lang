#include "incl/ast.h"
#include <cassert>
#include <iostream>
#include "plog/Log.h"

namespace zeal::ast {
void ast() {
  PLOGD << "Hello from ZAST!";
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
