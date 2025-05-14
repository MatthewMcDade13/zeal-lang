#pragma once



#include "common.h"
#include <algorithm>
#include <cstring>
#include <memory>
#include <type_traits>
#include <vector>
namespace zeal::core {

  
  /// copies given items of rvalue vec into 
  /// a shared_ptr<T[]>
  template<typename T>
  ArcVec<T> into_arcvec(std::vector<T>&& vec) {
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
  
}
