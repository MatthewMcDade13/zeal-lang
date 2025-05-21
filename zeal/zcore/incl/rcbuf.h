#pragma once



#include "common.h"
#include <algorithm>
#include <cstring>
#include <initializer_list>
#include <memory>
#include <type_traits>
#include <vector>
namespace zeal::core {

  
  /// copies given items of rvalue vec into 
  /// a shared_ptr<T[]>
  template<typename T>
  RcVec<T> into_arcvec(std::vector<T>&& vec) {
    auto* copied = new T[vec.size()](); 
    std::copy(vec.begin(), vec.end(), copied);

    // "drop" passed in vec
    vec.clear();
    return std::shared_ptr<T[]>(copied);
  }

  /// Reference Counted Buffer, acts as a light wrapper around std::shared_ptr<T[]>
  template<typename T>
  struct ArcBuf {

    using Inner = std::shared_ptr<T[]>;

    Inner ptr;


  };


  /// Non-Resizeable Shared-pointer array
  template<typename T>
  struct RcArray {

    std::shared_ptr<T[]> data;
    usize length;


    RcArray(usize size): data(std::make_shared<T[]>(size)), length(size) {}
    RcArray(std::initializer_list<T> list): data(std::make_shared<T[]>(list.size())), length(list.size()) { }
    RcArray(std::vector<T>&& vec): data(std::make_shared<T[]>(vec.size())), length(vec.size()) {
     memcpy(this->data.get(), vec.data(), sizeof(T) * vec.size()); 
    }


    T* begin() noexcept {
      return this->data.get();  
    }

    T* end() noexcept {
      return this->begin() + length;
    }

    const T* cbegin() const noexcept {
     return this->begin(); 
    }

    const T* cend() const noexcept {
      return this->end();
    }


    T& operator[](usize index) {
      auto* item = this->begin() + index;
      return *item; 
    }
    
  };
  
}
