#pragma once




#include "common.h"
#include <algorithm>
#include <cstring>
#include <memory>
#include <type_traits>
#include <variant>
namespace zeal::core {

  void zcore();


  template<typename T>
  concept Copyable = requires (T a, T b) { a = b; *a = *b; a[1] = *b; };


  template<typename T, u8 Size = 24>
  struct ShortBuf {
    static_assert(sizeof(T) <= 255, "Template type for Shortbuf, must not be larger than 255 bytes!"); 


    ShortBuf(): _buf(std::monostate()) {}

    ShortBuf(const std::array<T, Size>& buf) noexcept: _buf(buf) {}

    ShortBuf(std::span<const T> slice) {
      const usize len = slice.size();

      if (len >= Size) {
        auto sbuf[] = new T[len + 1]();
        std::memset(sbuf, '\0', len + 1);

        // _buf = std::make_shared<const T[]>(zeros);        
        std::memcpy(sbuf, slice.begin(), slice.end());
        _buf = std::shared_ptr<const T[]>(sbuf);
      } else {
        const std::array<T, Size> arr{0};
        std::memcpy(arr.get(), slice.begin(), len);
        _buf = arr;
      }
    }
        
    std::variant<std::monostate, std::array<T, Size>, std::shared_ptr<const T[]>> _buf{};  
  };
  

  using StrBuf = ShortBuf<u8>;

}
