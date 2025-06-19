#ifndef _ZEAL_ALLOC_INCL_STRING_POOL_H_
#define _ZEAL_ALLOC_INCL_STRING_POOL_H_

#include "common.h"
#include "mem.h"

ZEAL_CAPI_BEGIN

/// @breif StringPool Resource
/// @warning all of its fields are managed by zl_strpool_* functions.
/// public conumers of this struct should treat all its fields as if they were
/// readonly
///
typedef struct {
    /// Pointer to start of pool memory
    zl_MemoryBlock memory;

    /// byte offset to top of available pool memory.
    i32 top;

    /// Size of allocated memory pool in bytes
    i32 size_bytes;
    /// Count of string entries
    i32 count;
    ///  if true, pool will NOT be scanned for duplicate entries before inserting.
    ///  default behaviour is to scan for duplicates for inserting (allow_duplicates
    ///  = false)
    ///  @remarks If this field is changed after strings have already been inserted,
    ///  then only future additions will be unique/non-unique depending on the value
    ///  of this field
    bool allow_duplicates;
} zl_StringPool;

#define zl_stringpool_data(pool) zl_mempage_

extern zl_StringPool zl_stringpool_new(u8* begin, const i32 size_bytes,
                                       const bool zero_init);

/// calls zl_stringpool_new and passes false for zero_init flag
#define zl_stringpool_init(begin, size_bytes) \
    zl_stringpool_new(begin, size_bytes, false)

/// calls zl_stringpool_new and passes true for zero_init flag
#define zl_stringpool_zeroed(begin, size_bytes) \
    zl_stringpool_new(begin, size_bytes, true)

extern void zl_stringpool_reset(zl_StringPool* self, const bool zeroise);

/// wrapper for zl_stringpool_reset(self, true)
#define zl_stringpool_clear_zeroed(self) zl_stringpool_reset(self, true)

/// wrapper for zl_stringpool_reset(self, false)
#define zl_stringpool_clear(self) zl_stringpool_reset(self, false)

/// copies given c-string into stringpool. calls strlen over provided
/// string to determine its length.
/// if length is already known, or you don't want to copy the entire string,
/// @see [zl_stringpool_pushlen]
extern const char* zl_stringpool_push(zl_StringPool* self, const char* str);

/// Same as @see [zl_stringpool_push], but only copyies @param len bytes.
extern const char* zl_stringpool_pushlen(zl_StringPool* self, const char* str,
                                         const i32 len);

/// searches string pool for an entry that is equal to @param item.
/// @returns the address of the first match of string in string pool, otherwise
/// nullptr if no exact match is found
extern const char* zl_stringpool_find(const zl_StringPool* self, const char* item);

/// Checks if string pool has a string equal to @param item
/// @returns true if match found, false otherwise.
static inline bool zl_stringpool_contains(const zl_StringPool* self,
                                          const char* item) {
    const char* res = zl_stringpool_find(self, item);
    return nullptr != res;
}

ZEAL_CAPI_END

ZEAL_NAMESPACE(alloc)

#ifdef __cplusplus

using StringPool = zl_StringPool;

#endif  // ifdef __cplusplus

ZEAL_NAMESPACE_END

#endif  //_ZEAL_ALLOC_INCL_STRING_POOL_H_
