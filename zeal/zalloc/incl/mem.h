#ifndef _ZEAL_ALLOC_INCL_MEMORY_H_
#define _ZEAL_ALLOC_INCL_MEMORY_H_

#include <cstddef>

#include "common.h"
// #include "common.h"

ZEAL_CAPI_BEGIN
#define ZEAL_ENABLE_HUGEPAGES 0

#define VMEMORY_4KB (4ULL * 1024ULL)
#define VMEMORY_2MB (2ULL * 1024ULL * 1024ULL)
#define VMEMORY_1GB (1ULL * 1024ULL * 1024ULL * 1024ULL)

// #define VMEMORY_4KB 0
// #define VMEMORY_2MB 1
// #define VMEMORY_1GB 2

/// @brief 4KB multiplied by n (4KB * n)
/// Prefer to use these macros for initial reservation of virtual memory:
///
/// @code .cpp
///
/// u8* data = zl_reserve_memory(VMEMORY_4KBx(64));
///
/// @endcode
#define VMEMORY_4KBx(n) ((VMEMORY_4KB * n))
/// 2MB multiplied by n (2MB * n)
#define VMEMORY_2MBx(n) ((VMEMORY_2MB * n))
/// 1GB multiplied by n (1GB * n)
#define VMEMORY_1GBx(n) ((VMEMORY_1GB * n))

/// General ErrorState ErrorType info
enum zl_VMemErrorType {
    /// Someone somwhere trie to read/write protected memory! yikerinos!!!
    zl_VMemErrorType__InvalidMemoryAccess = -9,
    //// A call was made to commit virutal memory, but
    /// there is not enought available reserved virutal memory space
    /// to fulfill commit request.
    zl_VMemErrorType__OutOfReservedMemory = -8,
    /// Error occurred while attempting to decommit memory that may or may not
    /// have been previously reserved
    zl_VMemErrorType__DecommitFail = -7,
    /// Error occurred while attempting to release virtual page memory
    zl_VMemErrorType__ReleaseFail = -6,
    /// Error occurred while attempting to reserve virutal page memory
    /// If this happens and there is no parameter/configuration errors, then
    /// OS really strapped for memory and we should take this as our cue to BAIL!!!
    zl_VMemErrorType__ReserveFail = -5,
    /// Error occurred while attempting to commit virtual page memory that may or may
    /// not have
    /// been previously reserved. This covers all errors not caught by other Commit
    /// related error codes
    zl_VMemErrorType__CommitFail = -4,
    /// Something bad happened somewhere at some point in time in this abstract
    /// matrix of reality we call life... *burp*
    zl_VMemErrorType__Unknown = -3,
    /// system call requesting memory returned an error or nullptr! panic! abort!
    /// ahhhh!!
    zl_VMemErrorType__OOMAbort = -2,
    /// Invalid args passed to allocation function (ex: an unexpected nullptr,
    /// invalid page size, ect)
    zl_VMemErrorType__InvalidArgs = -1,

    /// no error
    zl_VMemErrorType__Ok = 0,
    /// Previous Allocation failed due to lack of
    /// committed virtual memory available. It would be nice to be able to check
    /// val >= zl_VMemErrorType__RequestResize
    /// and if so, the actual integer value is the minimum bytes required to ensure
    /// next allocation with same
    /// data succeeds.
    zl_VMemErrorType__RequestResize = 1,

};

/// @breif Gets OS Page Size
/// @returns u64 virtual memory page size from Operating System
extern u64 zl_get_page_size(void) ZEAL_NOEXCEPT;

/// @breif Reserves size_bytes memory from OS
/// @details Reserves a large, contiguous block of virtual address space.
/// @warning Does NOT commit any physical memory. The returned memory is not yet
/// accessible!!
/// @remarks @param size_bytes MUST be >= value returned by @see
/// [zalloc_get_page_size]. If it is not, this function does nothing and returns
/// nullptr/NULL
extern void* zl_vmemory_reserve_bytes(const i64 size_bytes) ZEAL_NOEXCEPT;

/// @breif Commits pages in a reserved block
/// @details memory becomes accessible (backed by physical pages)
extern i32 zl_vmemory_commit(void* memory, const i64 size_bytes) ZEAL_NOEXCEPT;

/// @breif Decommits pages
/// @details Decommits range of pages, releasing their physical memory.
/// @warning The address space at @param memory remains reserved, but becomes
/// inaccessible to read/writes
extern i32 zl_vmemory_decommit(void* memory, const i64 size_bytes) ZEAL_NOEXCEPT;

/// @breif Releases entire block of virutal memory back to operating system
extern i32 zl_vmemory_free(void* memory, const i64 size_bytes) ZEAL_NOEXCEPT;

/// @brief Virtual Memory Page metadata is stored on stack
/// with a small header before available storage
typedef struct {
    /// Root pointer to requested OS virtual memory page
    u8* begin;
    /// Pointer to end of requested OS virtual memory page
    u8* end;

    /// Count of calls to zl_commit_memory
    i32 commits;

    /// Count of currently allocated (committed) bytes
    i32 committed;

    /// Byte sum of all commits that have taken place since this MemoryBlocks
    /// initialization
    i32 sum_commit_bytes;

    /// Count of calls to zl_decommit_memory
    i32 decommits;
    /// number of bytes that have been decommitted
    i32 sum_decommit_bytes;
    /// number of bytes currently reserved that are
    /// available for committing
    i32 available;

    /// size of entire reserved virtual memory in bytes,
    /// Must be a power of 2 and/or a multiple of any of the 3
    /// valid page sizes (though most likely should just use 4KB page size...)
    i32 capacity;

    /// Memory alignment of reserved virtual memory in bytes.
    /// Ignored if <= 0.
    /// TODO: Actually set and use this field. for now i can't be tiffed lol
    i32 align;

#if ZEAL_ENABLE_HUGEPAGES
    /// Size in bytes of system huge page size to be  used
    /// when reserving and committing virtual memory.
    /// ignored if 0 and uses system page size for reserves/commits
    i32 huge_page_size;
#endif

} zl_MemoryBlock;

static ZEAL_FORCE_INLINE inline u8* zl_mblock_top(zl_MemoryBlock* self) ZEAL_NOEXCEPT {
    return self->begin + self->committed;
}

extern void zl_mblock_stats_string(const zl_MemoryBlock* mblock, char* buf,
                                   const i32 len, const bool pretty) ZEAL_NOEXCEPT;

extern void zl_mblock_print_stats(const zl_MemoryBlock* mblock) ZEAL_NOEXCEPT;

/// Creates a new MemoryBlock structure. Requests page_count of virtual memory
/// pages.
/// @param page_count Number of pages to reserve
/// @param commit_pages Number of pages to commit upfront
///
/// @remarks commit_pages ignored if 0. if < 0, commits all pages.
///
extern zl_MemoryBlock zl_mblock_new(const i32 page_count,
                                    const i32 commit_pages) ZEAL_NOEXCEPT;
/// Grows (Commits) mempage by size_bytes
extern i32 zl_mblock_push_bytes(zl_MemoryBlock* page,
                                const i64 size_bytes) ZEAL_NOEXCEPT;

/// Grows (commits) memory by count * page size
extern i32 zl_mblock_push_pages(zl_MemoryBlock* memory,
                                const i32 count) ZEAL_NOEXCEPT;

/// commits all reserved memory
extern i32 zl_mblock_full_commit(zl_MemoryBlock* memory) ZEAL_NOEXCEPT;

/// Shrinks (De-Commits) mempage by size_bytes
extern i32 zl_mblock_pop_bytes(zl_MemoryBlock* page,
                               const i64 size_bytes) ZEAL_NOEXCEPT;

/// Pops (Decommits) page size * count
extern i32 zl_mblock_pop_pages(zl_MemoryBlock* memory,
                               const i32 count) ZEAL_NOEXCEPT;
/// Deletes (Releases) mempage
extern i32 zl_mblock_free(zl_MemoryBlock* page) ZEAL_NOEXCEPT;

ZEAL_CAPI_END

ZEAL_NAMESPACE(alloc)

#ifdef __cplusplus

namespace os {
inline u64 get_page_size() noexcept {
    return zl_get_page_size();
}

template <typename T>
inline T* memory_reserve(const i64 size_bytes) noexcept {
    void* mem = zl_vmemory_reserve_bytes(size_bytes);
    return static_cast<T*>(mem);
}

template <typename T>
inline void memory_commit(T* memory, const i64 size_bytes) noexcept {
    zl_vmemory_commit(memory, size_bytes);
}

template <typename T>
inline void memory_decommit(T* memory, const i64 size_bytes) noexcept {
    zl_vmemory_decommit(memory, size_bytes);
}

template <typename T>
inline void memory_release(T* memory, const i64 size_bytes) noexcept {
    zl_vmemory_free(memory, size_bytes);
}
}  // namespace os

#endif

ZEAL_NAMESPACE_END

#endif
