#pragma once
#include <cstddef>
#include <cstdint>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using u8 = uint8_t;
using i8 = int8_t;
using u16 = uint16_t;
using i16 = int16_t;
using u32 = uint32_t;
using i32 = int32_t;
using u64 = uint64_t;
using i64 = int64_t;
using usize = std::size_t;
using isize = std::ptrdiff_t;

using f32 = float;
using f64 = double;

using String = std::string;

template<typename T>
using Vec = std::vector<T>;

using Buff = std::vector<u8>;

template<typename K, typename V>
using HashMap = std::unordered_map<K, V>;

template<typename V>
using HashSet = std::unordered_set<V>;
