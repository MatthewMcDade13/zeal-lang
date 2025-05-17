#pragma once

#include "rune.h"
#include "value.h"
#include <functional>
#ifndef ZVM_RUNTIME_STACK_SIZE
#define ZVM_RUNTIME_STACK_SIZE 4096
#endif

namespace zeal::core {
  
  // struct Stack {
  //   using ValResult = Result<std::reference_wrapper<Value>>;

  //   struct Slot {
  //     constexpr usize get() const noexcept {
  //       return index >= 0 ? index : std::numeric_limits<usize>::max() - 1;
  //     }

  //   private:
  //     friend Stack;
  //     constexpr Slot(usize id): index(id) {}
  //     const isize index{0};
  //   };

  //   Stack();


  //   ZvmResult<Slot> push(byte b) noexcept;
  //   ZvmResult<Slot> push(char c) noexcept;
  //   ZvmResult<Slot> push(i32 int32) noexcept;
  //   ZvmResult<Slot> push(i64 int64) noexcept;
  //   ZvmResult<Slot> push(f32 float32) noexcept;
  //   ZvmResult<Slot> push(f64 float64) noexcept;
  //   ZvmResult<Slot> push(BoxStr bs) noexcept;
  //   ZvmResult<Slot> push(const std::string& str) noexcept;
  //   ZvmResult<Slot> push(const std::string_view sv) noexcept;
  //   ZvmResult<Slot> push(Rune r) noexcept;
  //   ZvmResult<Slot> push(std::span<Value> vals) noexcept;
  //   ZvmResult<Slot> push(std::vector<Value>&& vals) noexcept;

  //   /// Index stack form relative to top
  //   ValResult peek_top(usize offset = 0) noexcept; 

  //   /// Index stack from relative to bottom 
  //   [[nodiscard]]
  //   ValResult lookup(Slot slot) noexcept;


  //   /// Index stack from relative to bottom
  //   [[nodiscard]]
  //   ValResult lookup(isize index) noexcept;    

  //   Value pop() noexcept;
  //   /// Returns span of popped. 
  //   /// @warning Returned span effectively points to invalid memory, as
  //   /// pointed to members will be invalidated when pushing items onto stack
  //   /// anytime after this call
  //   std::span<Value> pop_n(usize count) noexcept;

  // private:
  //   std::array<Value, ZVM_RUNTIME_STACK_SIZE> mem;
  //   usize top;
  // };
}
