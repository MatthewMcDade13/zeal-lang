#pragma once

#include "common.h"
#include "rune.h"
#include "slice.h"
#include <any>
namespace zeal::core {

struct Value final {

  struct None final {};
  struct Struct final {};

  enum class Type : u8 {
    Uninit,
    None,
    Byte,
    Character,
    Int32,
    Int64,
    Float32,
    Float64,
    Rune,
    String,
    List,
    Pointer,
    HashMap,
  };

  constexpr Value(): data(std::monostate()) {}
  constexpr Value(byte b): data(b) {}
  constexpr Value(char c): data(c) {}
  constexpr Value(i32 int32_in): data(int32_in) {}
  constexpr Value(i64 int64_in): data(int64_in) {}
  constexpr Value(f32 float32_in): data(float32_in) {}
  constexpr Value(f64 float64_in): data(float64_in) {}
  constexpr Value(Rune r): data(r) {}
  constexpr Value(BoxStr bs): data(bs) {}
  constexpr Value(Slice<Value> list_in): data(list_in) {}
  Value(std::vector<Value>&& list_in);

  Value(const Value& other);
  Value(Value&& other);
  Value& operator=(const Value& other);
  Value& operator=(Value&& other);


  constexpr Type get_type() const noexcept {

    static constexpr Type TYPE_INDEX[] = {
        Type::Uninit, Type::Byte,    Type::Character, Type::Int32,
        Type::Int64,  Type::Float32, Type::Float64,   Type::Rune,
        Type::String, Type::List,    Type::Pointer, Type::HashMap};

    const auto index = this->data.index();
    return TYPE_INDEX[index];
  }

  static Value make_rune(const std::string& rune);
  static Value make_string(const std::string& string);

  Opt<byte> try_read_byte() const noexcept;
  Opt<char> try_read_character() const noexcept;
  Opt<i32> try_read_int32() const noexcept;
  Opt<i64> try_read_int64() const noexcept;
  Opt<f32> try_read_float32() const noexcept;
  Opt<f64> try_read_float64() const noexcept;
  Opt<Rune> try_read_rune() const noexcept;
  Opt<Slice<Value>> try_read_list() const noexcept;

  byte expect_byte() const;
  char expect_character() const;
  i32 expect_int32() const;
  i64 expect_int64() const;
  f32 expect_float32() const;
  f64 expect_float64() const;
  Rune expect_rune() const;
  Slice<Value> expect_list() const;

  bool try_set_byte(byte b) noexcept;
  bool try_set_character(char c) noexcept;
  bool try_set_i32(i32 int32) noexcept;
  bool try_set_i64(i64 int64) noexcept;
  bool try_set_f32(f32 float32) noexcept;
  bool try_set_f64(f64 float64) noexcept;
  bool try_set_rune(Rune rune) noexcept;
  bool try_set_rune(const std::string& rune);
  bool try_set_string(BoxStr str) noexcept;
  bool try_set_string(const std::string& string);
  bool try_set_list(Slice<Value> list);
  bool try_set_list(std::vector<Value>&& list) noexcept;
  bool try_set_list(Value* values, usize count);

  void write_byte(byte b);
  void write_character(char c);
  void write_none();
  void write_i32(i32 int32);
  void write_i64(i64 int64);
  void write_f32(f32 float32);
  void write_f64(f64 float64);
  void write_rune(Rune rune);
  void write_rune(const std::string& rune);
  void write_string(BoxStr string);
  void write_string(const std::string& string);
  void write_list(Slice<Value> list);
  void write_list(std::vector<Value> list);
  void write_list(Value* values, usize count);

  void reset();

private:
  using OptPointer = std::optional<std::shared_ptr<Value>>;
  std::variant<std::monostate, None, byte, char, i32, i64, f32, f64, Rune,
               BoxStr, Slice<Value>, Struct, std::any*>
      data;
};

} // namespace zeal::core
