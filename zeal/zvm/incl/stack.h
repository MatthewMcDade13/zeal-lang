#pragma once

#include "common.h"
#include <cstring>
namespace zeal::zvm {

#ifndef ZVM_STACK_MEMORY
#define ZVM_STACK_MEMORY 4096
#endif

template <usize Size> struct Stack {


  template <typename T>
  void push(T item) { this->push(&item, sizeof(T)); }

  void push(void* data, usize size_bytes) {
    auto* top = &memory[top_slot + 1];
    std::memcpy(top, data, size_bytes);
    this->memory[top_slot + size_bytes + 1] = this->top_slot;

  }

  template <typename T> std::optional<T*> peek_top() {
    if (this->top_slot == 0) {
      return std::nullopt;
    }

    auto* top = &memory[top_slot - 1];
  }

private:
  std::array<i8, Size> memory{};
  usize top_slot{};
};

/// Stack with size of ZVM_STACK_MEMORY
using DefaultStack = Stack<ZVM_STACK_MEMORY>;

} // namespace zeal::zvm
