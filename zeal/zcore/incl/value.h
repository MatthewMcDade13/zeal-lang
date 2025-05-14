#pragma once

#include "common.h"
#include "rune.h"
#include "slice.h"
namespace zeal::core {


struct Value {
  enum class Type : u8 {
    Byte,
    Character,
    Int32,
    Float32,
    Int64,
    Float64,
    Rune,
    String,
    List,
  };

  Value();
  Value(byte b);
  Value(char c);
  Value(i32 int32_in);
  Value(i64 int64_in);
  Value(f32 float32_in);
  Value(f64 float64_in);
  Value(Rune r);
  Value(BoxStr bs);
  Value(Slice<Value> list_in);

  Value(std::vector<Value>&& list_in);

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




private:
  union {
    byte byte;
    char character;
    i32 int32;
    i64 int64;
    f32 float32;
    f64 float64;
    Rune rune;
    BoxStr string;
    Slice<Value> list;
  } data;


  Type type;

};

} // namespace zeal::core
