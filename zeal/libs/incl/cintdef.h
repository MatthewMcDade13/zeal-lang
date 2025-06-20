#ifndef _ZEAL_LIBS_INCL_CINTDEF_H_
#define _ZEAL_LIBS_INCL_CINTDEF_H_

#ifdef __cplusplus
#include <cstddef>
#include <cstdint>
#else
#include <stddef>
#include <stdint>
#endif

#ifdef __cplusplus

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

#else
typedef uint8_t u8;
typedef int8_t i8;
typedef unsigned char byte;
typedef uint16_t u16;
typedef int16_t i16;
typedef int32_t i32;
typedef uint32_t u32;
typedef int64_t i64;
typedef uint64_t u64;

typedef size_t usize;
typedef ptrdiff_t isize;

typedef float f32;
typedef double f64;
#endif

#endif
