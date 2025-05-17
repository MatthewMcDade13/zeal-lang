#include "stack.h"


namespace zeal::core {
  // Stack::Stack() {}
  // using SSlot = Stack::Slot;
  // using SlotResult = Result<SSlot>;
  // using ValResult = Stack::ValResult;



  //   SlotResult Stack::push(byte b) noexcept {
  //     if (this->top >= this->mem.max_size()) {

  //       // return ZvmError();
  //     }
  //     const auto slot = Slot(this->top);

  //   }

  //   SlotResult Stack::push(char c) noexcept {}
  //   SlotResult Stack::push(i32 int32) noexcept {}
  //   SlotResult Stack::push(i64 int64) noexcept {}
  //   SlotResult Stack::push(f32 float32) noexcept {}
  //   SlotResult Stack::push(f64 float64) noexcept {}
  //   SlotResult Stack::push(BoxStr bs) noexcept {}
  //   SlotResult Stack::push(const std::string& str) noexcept {}
  //   SlotResult Stack::push(const std::string_view sv) noexcept {}
  //   SlotResult Stack::push(Rune r) noexcept {}
  //   SlotResult Stack::push(std::span<Value> vals) noexcept {}
  //   SlotResult Stack::push(std::vector<Value>&& vals) noexcept {}

  //   /// Index stack form relative to top
  //   ValResult Stack::peek_top(usize offset) noexcept {} 

  //   /// Index stack from relative to bottom 
  //   [[nodiscard]]
  //   ValResult Stack::lookup(SSlot slot) noexcept {}


  //   /// Index stack from relative to bottom
  //   [[nodiscard]]
  //   ValResult Stack::lookup(isize index) noexcept {}    

  //   Value Stack::pop() noexcept {}
  //   /// Returns span of popped. 
  //   /// @warning Returned span effectively points to invalid memory, as
  //   /// pointed to members will be invalidated when pushing items onto stack
  //   /// anytime after this call
  //   std::span<Value> Stack::pop_n(usize count) noexcept {}


  
}
