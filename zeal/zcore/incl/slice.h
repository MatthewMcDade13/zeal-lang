#pragma once

#include "common.h"
#include <memory>

namespace zeal::core {

  template<typename T>
  struct Slice {
    std::shared_ptr<T[]> ptr;
    usize length;
  };
  
}
