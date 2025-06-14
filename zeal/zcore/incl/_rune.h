#pragma once

#include "common.h"
#include <concepts>
#include <cstring>
#include <memory>
#include <span>
#include <string_view>
#include <variant>

namespace zeal::core {

struct BoxStr {
  const std::shared_ptr<const char[]> bytes{nullptr};
  constexpr BoxStr(std::shared_ptr<const char[]> sptr = nullptr) noexcept
      : bytes(sptr) {}

  /// Ad-Hoc move constructor from string.
  static constexpr BoxStr from_string(std::string&& string) noexcept {
    auto s = reinterpret_cast<char*>(new u8[string.size()]());
    std::strcpy(s, string.c_str());
    auto _ = std::move(string);
    return BoxStr(std::shared_ptr<const char[]>(s));
  }

  static BoxStr create(const std::string_view sv) {
    auto buf = new char[sv.size()]();
    std::strcpy(buf, sv.data());
    auto bptr = std::shared_ptr<const char[]>(buf);
    return BoxStr(bptr);
  }

  constexpr bool is_empty() const noexcept { return nullptr != bytes; }

  constexpr bool has_some() const noexcept { return !is_empty(); }

  constexpr std::string_view as_view() const noexcept {
    if (this->is_empty())
      return {};

    auto ptr = (const char*) this->bytes.get();
    return std::string_view{ptr};
  }

  std::span<const char> as_span() const noexcept {
    if (this->is_empty())
      return {};
    const auto len = std::strlen(this->bytes.get());
    return std::span{this->bytes.get(), len};
  }
};

/// Unique(ish) String.
/// Runes are not guaranteed to be runtime unique, but there is
/// a pretty good chance that it is.
/// @see [Runeword] for Runes that are guaranteed to be unique
/// at runtime. (ZVM allocates all Runewords during init in a contiguous block
/// of readonly pinned memory)
struct Rune {

  static constexpr const usize MAX_LEN = 16;

  using Empty = std::monostate;
  using SubString = std::string_view;
  using Inline = std::array<u8, MAX_LEN>;
  using Inner = std::variant<Empty, Inline, BoxStr, SubString>;
  using Self = Rune;

  // union {
  //  std::array<u8, MAX_LEN> small;
  //  BoxStr large;
  // } _buf;

  constexpr Rune() noexcept : _buf() {}

  // String(std::array<u8, )

  template<const char str[]>
  static constexpr Rune from_static() noexcept {
    constexpr const isize len = sizeof(str) - 1;
    if (len <= 0) {
      return Rune();
    }

    Rune r;
    r._buf = std::string_view(str);
    return r;
  }

  static constexpr Rune from_static(const std::string_view static_string) noexcept {
    if (static_string.size() == 0) {
      return Rune();
    }
    Rune r;
    r._buf = std::string_view(static_string);
    return r;
  }


  
  [[nodiscard]]
  static Rune from(std::string_view src) {
    const auto strlen = src.size();

    if (strlen == 0)
      return Rune();
    if (strlen >= MAX_LEN) {
      std::string sbuf{src};
      auto bs = BoxStr::from_string(std::move(sbuf));
      return Rune(std::move(bs));
    } else {
      Inline arr{};
      std::memcpy(arr.data(), src.cbegin(), std::min(src.size(), MAX_LEN));
      return Rune(arr);
    }
  }

  [[nodiscard]]
  const char& operator[](const usize index) const& noexcept {

    if (index >= MAX_LEN || index >= sview().size()) {
      constexpr static const char space{' '};
      return space;
    } else {
      return sview()[index];
    }
  }

  [[nodiscard]]
  static constexpr Rune make_default() noexcept {
    return {};
  }

  [[nodiscard]]
  static constexpr Rune none() noexcept {
    return Rune::make_default();
  }

  [[nodiscard]]
  constexpr std::string_view sview() const noexcept {
    if (auto* res = std::get_if<BoxStr>(&_buf)) {
      return res->as_view();
    } else if (auto res = std::get_if<SubString>(&_buf)) {
      return std::string_view{(const char*) res->data(), res->size()};
    } else if (auto res = std::get_if<Inline>(&_buf)) {
      return std::string_view{(const char*) res->data(), res->size()};
    } else {
      return {};
    }
  }

  /// Return pointer is guaranteed to be non-null if optional has a value
  [[nodiscard]] constexpr std::optional<const char*> data() const noexcept {
    const auto view = this->sview();
    if (view.size() > 0 && view.data() != nullptr) {
      return view.data();
    } else {
      return std::nullopt;
    }
  }

  constexpr operator std::string_view() const noexcept { return this->sview(); }

private:
  Rune(BoxStr&& string) : _buf(std::move(string)) {}
  Rune(SubString sub) : _buf(sub) {}
  Rune(Inline arr) : _buf(arr) {}

  /// Reference to the root of an allocated string (std::shared_ptr<const u8[]>
  /// OR std::array<u8, MAX_LEN - 1>)
  Inner _buf;
};

namespace traits {

// constexpr Rune default() {}

using std::operator""s;
using std::operator""sv;
/// Rune-like value trait
template <typename T>
concept Runic = (requires (T a) {
  { a.as_rune() } -> std::convertible_to<Rune>;
} || requires(T a) {
  { a->as_rune() } -> std::convertible_to<Rune>;
}) && requires { T{"string"}; T{""s}; T{""sv}; T{Rune::make_default()}; };

} // namespace traits

} // namespace zeal::core
