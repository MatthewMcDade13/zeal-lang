#ifndef _ZEAL_ALLOC_INCL_STRING_POOL_H_
#define _ZEAL_ALLOC_INCL_STRING_POOL_H_

#include "common.h"
#include "mem.h"

ZEAL_CAPI_BEGIN

/// @brief StringPool Resource
/// @remarks Allocates a page of memory, commits as it grows,
///
typedef struct {
    /// Pointer to start of pool memory
    zl_MemoryBlock memory;

    /// Byte offset from start of pool memory to
    /// end of last string in pool
    /// This has nothing to do with memory capacity, its just
    /// a easy way to get to top of next area of reserved and committed memory to
    /// write to, instead of storing a pointer.
    i32 len_bytes;

    /// Count of string entries
    i32 count;
} zl_StringPool;

/// Creates a new StringPool
/// @param size_bytes initall vmem commit size, if < 0, commits full 4KB page
extern zl_StringPool zl_stringpool_new(const i32 size_bytes) ZEAL_NOEXCEPT;

///
extern void zl_stringpool_reset(zl_StringPool* self,
                                const bool zeroise) ZEAL_NOEXCEPT;

/// wrapper for zl_stringpool_reset(self, true)
#define zl_stringpool_clear_zeroed(self) zl_stringpool_reset(self, true)

/// wrapper for zl_stringpool_reset(self, false)
#define zl_stringpool_clear(self) zl_stringpool_reset(self, false)

/// copies given c-string into stringpool. calls strlen over provided
/// string to determine its length.
/// if length is already known, or you don't want to copy the entire string,
/// @see [zl_stringpool_pushlen]
///
/// @remarks The pointer returned by this function shall be valid for
/// the enitre lifetime of the StringPool.
extern const char* zl_stringpool_push(zl_StringPool* self,
                                      const char* str) ZEAL_NOEXCEPT;

/// Same as @see [zl_stringpool_push], but only copyies @param len bytes.
extern const char* zl_stringpool_pushlen(zl_StringPool* self, const char* str,
                                         const i32 len) ZEAL_NOEXCEPT;

/// Free memory associated with this stringpool
extern void zl_stringpool_free(zl_StringPool* self) ZEAL_NOEXCEPT;

ZEAL_CAPI_END

ZEAL_NAMESPACE(alloc)

#ifdef __cplusplus

using StringPool = zl_StringPool;

#endif  // ifdef __cplusplus

ZEAL_NAMESPACE_END

#endif  //_ZEAL_ALLOC_INCL_STRING_POOL_H_
