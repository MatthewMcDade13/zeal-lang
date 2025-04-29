#pragma once
#include "../typedefs.h"

typedef struct ZvmSlice {
  u8 *begin;
  usize size_bytes;
  usize stride;
} ZvmSlice;

