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
  const std::shared_ptr<const u8[]> bytes{nullptr};
  const usize len{0};

  constexpr BoxStr(std::shared_ptr<const u8[]> sptr = nullptr, usize inlen = 0) noexcept: bytes(sptr), len(inlen) {}

  /// Ad-Hoc move constructor from string.
  static constexpr BoxStr from_string(std::string&& string) noexcept {
    auto s = reinterpret_cast<char*>(new u8[string.size()]());
    std::strcpy(s, string.c_str());
    auto _ = std::move(string);
    const u8* res = reinterpret_cast<u8*>(s);
    return BoxStr(std::shared_ptr<const u8[]>(res), string.size());
  }


  static BoxStr create(const std::string_view sv) {
    auto buf = new u8[sv.size()]();
    std::memcpy(buf, sv.data(), sv.size());
    auto bptr = std::shared_ptr<const u8[]>(buf);    
    return BoxStr(bptr, sv.size());

  }

  

  constexpr bool is_empty() const noexcept {
    return nullptr != bytes && len > 0;
  }

  constexpr bool has_some() const noexcept {
    return !is_empty();
  }

  constexpr std::string_view as_view() const noexcept {
   if (this->is_empty()) return {};

   auto ptr = (const char*)this->bytes.get();
   return std::string_view{ptr, this->len}; 
  }

  constexpr std::span<const u8> as_span() const noexcept {
    if (this->is_empty()) return {}; 
    return std::span{this->bytes.get(), this->len};
  }

};

/// Unique(ish) String.
/// Runes are not guaranteed to be runtime unique, but there is
/// a pretty good chance that it is.
/// @see [Runeword] for Runes that are guaranteed to be unique
/// at runtime. (ZVM allocates all Runewords during init in a contiguous block
/// of readonly pinned memory)
struct Rune {

  static constexpr const usize MAX_LEN = sizeof(size_t) * 3;

  using Empty = std::monostate;
  using SubString = std::span<const u8>;
  using Inline = std::array<u8, MAX_LEN>;
  using Inner = std::variant<Empty, Inline, BoxStr, SubString>;
  using Self = Rune;

 constexpr Rune() noexcept : _buf(Empty()) {}

  // String(std::array<u8, )

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
  const char& operator[](const usize index) const & noexcept {  

    if (index >= MAX_LEN || index >= sview().size()) {
     constexpr static const char space{' '}; 
     return space;
    } else {
      return sview()[index];
    }
  }


  [[nodiscard]]
  static constexpr Rune make_default() noexcept { return {}; }

  [[nodiscard]]
  static constexpr Rune none() noexcept { return Rune::make_default(); } 

  [[nodiscard]]
  constexpr std::string_view sview() const noexcept {
    if (auto* res = std::get_if<BoxStr>(&_buf)) {
      return res->as_view();
    } else if (auto res = std::get_if<SubString>(&_buf)) {
      return std::string_view{(const char*)res->data(), res->size()};
    } else if (auto res = std::get_if<Inline>(&_buf)) {
      return std::string_view{(const char*) res->data(), res->size()};
    } else {
      using std::operator""sv;
      return ""sv;      
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

  constexpr operator std::string_view() const noexcept {
    return this->sview();
  }


private:


  Rune(BoxStr&& string): _buf(std::move(string)) {}
  Rune(SubString sub): _buf(sub) {}
  Rune(Inline arr): _buf(arr) {}

  
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

  
}
