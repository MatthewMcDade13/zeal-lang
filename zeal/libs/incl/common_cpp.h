#pragma once

// #include <algorithm>
// #include <any>
// #include <concepts>
// #include <cstddef>
// #include <cstdint>
// #include <cstring>
// #include <expected>
// #include <functional>
// #include <iterator>
// #include <memory>
// #include <span>
// #include <sstream>
// #include <string_view>
// #include <tuple>
// #include <type_traits>
// #include <unordered_map>
// #include <unordered_set>
// #include <variant>
// #include <vector>

#include <cstddef>
#include <cstdint>
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

// template <typename T>
// using Opt = std::optional<T>;

// template <typename T>
// using cslice = std::span<const T>;

// template <typename T>
// using slice = std::span<T>;

// using Bytes = slice<byte>;

// template <typename T>
// using Vec = std::vector<T>;
// using String = std::string;
// /// String view, similar to rust's &str
// /// @warning !!! this does not handle termination of strings as               !!!
// /// !!! this is just a pointer and a length, so if passed to C/C++ apis, that !!!
// /// !!! expect a C null termined string, bad things will happen lol           !!!
// using Str = std::string_view;

// /// String view, similar to rust's &str
// /// @warning !!! this does not handle termination of strings as               !!!
// /// !!! this is just a pointer and a length, so if passed to C/C++ apis, that !!!
// /// !!! expect a C null termined string, bad things will happen lol           !!!
// using Sview = std::string_view;

// /// String view, similar to rust's &str
// /// @warning !!! this does not handle termination of strings as               !!!
// /// !!! this is just a pointer and a length, so if passed to C/C++ apis, that !!!
// /// !!! expect a C null termined string, bad things will happen lol           !!!
// using Sref = std::string_view;

// template <typename A, typename B>
// using Tup2 = std::tuple<A, B>;

// template <typename A, typename B, typename C>
// using Tup3 = std::tuple<A, B, C>;

// template <typename K, typename V>
// using HashMap = std::unordered_map<K, V>;

// template <typename T>
// using HashSet = std::unordered_set<T>;

// template <typename T>
// using Rc = std::shared_ptr<T>;

// template <typename T>
// using Box = std::unique_ptr<T>;

// template <typename T>
// using RcVec = std::shared_ptr<T[]>;

// template <typename... Args>
// using Union = std::variant<Args...>;

// /// Type tag for unit type. Essentially a marker denoting "nothing"
// /// or any other abstract equivalent objects (NOTE: This is word salad, and im
// /// keeping the comment as it is for lolz :D)
// struct UnitTag final {};
// using TUnit = UnitTag;

// /// A general/any error.
// /// simply a templated Error Type and
// /// a static sized message buffer
// template <usize MessageLen = 255, typename ErrorCode = i32>
// struct Error {
//     /// Error value for anything, refer to
//     /// message field for error information
//     static constexpr const ErrorCode ANY = 0;
//     ErrorCode error_code;
//     std::array<char, MessageLen> message;
//     // char message[MessageLen];

//     constexpr Error() noexcept : error_code({}), message({}) {}
//     constexpr Error(const ErrorCode err) noexcept : error_code(err), message({}) {}
//     constexpr Error(const ErrorCode err, const Str msg) noexcept : Error(err) {
//         const auto len = std::min(MessageLen, msg.size());
//         std::memcpy(this->message.data(), msg.data(), len - 1);
//         this->message[len] = '\0';
//     }

//     constexpr String to_string() const {
//         std::stringstream ss;
//         const auto msg = std::string(this->message.data());

//         ss << "Error(" << std::to_string(this->error_code) << ") => " << msg << "\n";
//         return ss.str();
//     }
// };

// using Err = Error<255>;

// struct UnitType {
    
// };

// /// Type alias for C++23 std::expected.
// /// @template T must be copy-constructible and not a reference type.
// /// @see [ResultRef] if you need T to be a ref type.
// template <typename T = UnitType>
// using Result = std::expected<T, Err>;

// using IOResult = Result<TUnit>;

// /// Type alias for C++23 std::expected.
// /// @template T must be a reference type
// /// @see [Result] if you need a Result with default behavior
// template <typename T>
// using ResultRef = Result<std::reference_wrapper<T>>;

// constexpr Err make_error(const i32 code, const Str message = "") noexcept {
//     return Err(code, message);
// }

// constexpr Err make_error(const Str message) noexcept {
//     return make_error(Err::ANY, message);
// }

// #ifndef ZERR
// /// Conveinence wrapper around std::unexpected(make_error(...))
// #define ZERR(...) std::unexpected(make_error(__VA_ARGS__))
// #endif

// template <typename T>
// constexpr auto me(const i32 code) -> Result<T> {
//     return std::unexpected(make_error(code));
// }

// namespace type {

// template <typename T>
// struct Any {};

// }  // namespace type

}  // namespace zeal
