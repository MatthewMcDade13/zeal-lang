#pragma once

#include <any>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <functional>
#include <iterator>
#include <memory>
#include <span>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>
#include <expected>

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
using f32 = float;
using f64 = double;

template <typename T> using Opt = std::optional<T>;

template <typename T> using slice = std::span<const T>;

template <typename T> using slice_mut = std::span<T>;

using Bytes = slice_mut<byte>;

template <typename T> using Vec = std::vector<T>;

template <typename K, typename V> using HashMap = std::unordered_map<K, V>;

template <typename T> using HashSet = std::unordered_set<T>;

template <typename T> using Arc = std::shared_ptr<T>;

template <typename T> using Box = std::unique_ptr<T>;

template <typename T> using ArcVec = std::shared_ptr<T[]>;

struct ErrorInfo {
  i32 error_code;
  char message[255];  
};

template <typename T>
using Result = std::expected<std::reference_wrapper<T>, ErrorInfo>;

/// Concept for callback functions that take a single const data parameer and return a template arg [Result]
template<typename Callback, typename Result, typename Param = std::any>
concept PredicateAny = requires(Callback cb, const Param& t) {
  { cb(t) } -> std::convertible_to<Result>;
};



} // namespace zeal
