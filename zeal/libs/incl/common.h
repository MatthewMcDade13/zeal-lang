#ifndef _ZEAL_CORE_INCL_C_COMMON_H_
#define _ZEAL_CORE_INCL_C_COMMON_H_

#include "cintdef.h"

#ifdef __cplusplus
#include "common_cpp.h"

#define ZEAL_NAMESPACE(NAME) namespace zeal::NAME {
#define ZEAL_NAMESPACE_END }

#define ZEAL_CAPI_BEGIN extern "C" {
#define ZEAL_CAPI_END }

#define ZEAL_NOEXCEPT noexcept

#else
#include "common_c.h"

#define ZEAL_NAMESPACE(NAME)
#define ZEAL_NAMESPACE_END
#define ZEAL_CAPI_BEGIN
#define ZEAL_CAPI_END
#define ZEAL_NOEXCEPT

#endif  // __cplusplus

// NOTE: I hate windows... why... just... why...
#ifdef _WIN32
#ifdef ZEAL_LIBRARY_IMPLEMENTATION
#define ZEAL_API __declspec(dllexport)
#else
#define ZEAL_API __declspec(dllimport)
#endif  // ifdef ZEAL_LIBRARY_IMPLEMENTATION
#else   // non-windows platform
#define ZEAL_API __attribute__((visibility("default")))
#endif  // ifdef _WIN32

#ifdef _MSC_VER
#define ZEAL_FORCE_INLINE __forceinline
#elif defined(__GNUC__) || defined(__clang__)
#define ZEAL_FORCE_INLINE __attribute__((always_inline))
#else
#define ZEAL_FORCE_INLINE
#endif

#define zl_printf(format, ...) (printf(format, __VA_ARGS__))

#if defined(__clang__) || defined(__GNUC__)
#define Zeal_Unreachable() __builtin_unreachable()
#define Zeal_Panic(fmt, ...)      \
    do {                          \
        printf(fmt, __VA_ARGS__); \
        __builtin_trap();         \
    } while (0)
#elif defined(_MSC_VER)
#define Zeal_Unreachable() __assume(0)
#define Zeal_Panic(fmt, ...)      \
    do {                          \
        printf(fmt, __VA_ARGS__); \
        __debugbreak();           \
    } while (0)
#else  // non clang,gnu or msvc platform
#define Zeal_Unreachable()
#define Zeal_Panic(fmt, ...)      \
    do {                          \
        printf(fmt, __VA_ARGS__); \
        (*(volatile int*) 0 = 0); \
                                  \
    } while (0)
#endif  //
#endif  // header guard
