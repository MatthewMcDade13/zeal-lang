#pragma once


#include "common.h"
namespace zeal::core::conv {
  constexpr u32 le_bytes_to_uint32(std::array<u8, sizeof(u32)> bytes) {
   // Conversion
    uint32_t res = 0;

    // Little-endian:
    res |= static_cast<uint32_t>(bytes[0]);
    res |= static_cast<uint32_t>(bytes[1]) << 8;
    res |= static_cast<uint32_t>(bytes[2]) << 16;
    res |= static_cast<uint32_t>(bytes[3]) << 24;    
    return res;

  }
 
  constexpr u32 le_bytes_to_uint32(const u8 a, const u8 b, const u8 c, const u8 d) {
    return le_bytes_to_uint32({a, b,c,d});
  }

  constexpr u32 be_bytes_to_uint32(std::array<u8, sizeof(u32)> bytes) {

    uint32_t res = 0;
    res |= static_cast<uint32_t>(bytes[0]) << 24;
    res |= static_cast<uint32_t>(bytes[1]) << 16;
    res |= static_cast<uint32_t>(bytes[2]) << 8;
    res |= static_cast<uint32_t>(bytes[3]);
    return res; 
  }

  constexpr u32 be_bytes_to_uint32(const u8 a, const u8 b, const u8 c, const u8 d) {
    return be_bytes_to_uint32({a, b,c,d});
  }


}
