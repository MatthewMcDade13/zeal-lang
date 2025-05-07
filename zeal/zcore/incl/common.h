#pragma once

#include <cstddef>
#include <cstdint>
#include <span>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace zeal {
  using u8 = uint8_t;
  using i8 = int8_t;
  using byte = std::byte;
  using u16 = uint16_t;
  using i16 = int16_t;
  using i32 = int32_t;
  using u32 = uint32_t;
  using i64 = int64_t;
  using u64 = uint64_t;
  using usize = size_t;
  using isize = long; 

  template<typename T>
  using slice = std::span<const T>;

  template<typename T>
  using slice_mut = std::span<T>;

  using Bytes = slice_mut<byte>;

  template<typename T>
  using Vec = std::vector<T>;

  template<typename K, typename V>
  using HashMap = std::unordered_map<K, V>;

  template<typename T>
  using HashSet = std::unordered_set<T>;
}
