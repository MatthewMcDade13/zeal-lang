#pragma once

#include <any>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <expected>
#include <functional>
#include <iterator>
#include <algorithm>
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

template <typename T>
using Opt = std::optional<T>;

template <typename T>
using slice = std::span<const T>;

template <typename T>
using slice_mut = std::span<T>;

using Bytes = slice_mut<byte>;

template <typename T>
using Vec = std::vector<T>;
using String = std::string;
/// String view, similar to rust's &str
/// @warning !!! this does not handle termination of strings as               !!!
/// !!! this is just a pointer and a length, so if passed to C/C++ apis, that !!!
/// !!! expect a C null termined string, bad things will happen lol           !!!
using Str = std::string_view;

/// String view, similar to rust's &str
/// @warning !!! this does not handle termination of strings as               !!!
/// !!! this is just a pointer and a length, so if passed to C/C++ apis, that !!!
/// !!! expect a C null termined string, bad things will happen lol           !!!
using Sview = std::string_view;

/// String view, similar to rust's &str
/// @warning !!! this does not handle termination of strings as               !!!
/// !!! this is just a pointer and a length, so if passed to C/C++ apis, that !!!
/// !!! expect a C null termined string, bad things will happen lol           !!!
using Sref = std::string_view;

template<typename A, typename B>
using Tup2 = std::tuple<A, B>;

template<typename A, typename B, typename C>
using Tup3 = std::tuple<A, B, C>;

template <typename K, typename V>
using HashMap = std::unordered_map<K, V>;

template <typename T>
using HashSet = std::unordered_set<T>;

template <typename T>
using Rc = std::shared_ptr<T>;

template <typename T>
using Box = std::unique_ptr<T>;

template <typename T>
using RcVec = std::shared_ptr<T[]>;

template<typename ...Args>
using Union = std::variant<Args...>;

struct ErrorInfo {
    i32 error_code;
    char message[255];
};


template <typename T>
using Result = std::expected<T, ErrorInfo>;


template <typename T>
using ResultRef = Result<std::reference_wrapper<T>>;



}  // namespace zeal
