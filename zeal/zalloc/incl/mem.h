#ifndef _ZEAL_ALLOC_INCL_MEMORY_H_
#define _ZEAL_ALLOC_INCL_MEMORY_H_

#include <cstddef>

#include "common.h"
// #include "common.h"

ZEAL_CAPI_BEGIN

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

/// Used to track the size of pages, constants correlate with thier numeric byte
/// values for debugging
enum zl_VMemType {
    VMemType_1GB = 1,
    VMemType_2MB = 2,
    VMemType_4KB = 4,
};

/// @breif Gets OS Page Size
/// @returns u64 virtual memory page size from Operating System
extern i64 zl_get_page_size(void) ZEAL_NOEXCEPT;

/// @breif Reserves size_bytes memory from OS
/// @details Reserves a large, contiguous block of virtual address space.
/// @warning Does NOT commit any physical memory. The returned memory is not yet
/// accessible!!
/// @remarks @param size_bytes MUST be >= value returned by @see
/// [zalloc_get_page_size]. If it is not, this function does nothing and returns
/// nullptr/NULL
extern void* zl_reserve_memory(const i64 size_bytes) ZEAL_NOEXCEPT;

/// @breif Commits pages in a reserved block
/// @details memory becomes accessible (backed by physical pages)
extern int zl_commit_memory(void* memory, const i64 size_bytes) ZEAL_NOEXCEPT;

/// @breif Decommits pages
/// @details Decommits range of pages, releasing their physical memory.
/// @warning The address space at @param memory remains reserved, but becomes
/// inaccessible to read/writes
extern int zl_decommit_memory(void* memory, const i64 size_bytes) ZEAL_NOEXCEPT;

/// @breif Releases entire block of virutal memory back to operating system
extern int zl_release_memory(void* memory, const i64 size_bytes) ZEAL_NOEXCEPT;

typedef struct zl_BlockChunk {
    /// Number of bytes currently committed in this memory chunk
    /// We shall keep this field first as it will most likely be the most frequently
    /// accessed in this header, which also allows us to get current committed byte
    /// count through something like:
    ///
    /// @code {.cpp}
    ///
    ///
    /// const auto committed =
    /// *(reinterpret_cast<i32*>(memory_block->head))
    ///
    ///
    /// @endcode
    ///
    ///
    i32 committed;

    /// size of entire reserved virtual memory in bytes,
    /// Must be on of the VMEMORY_SIZE_* variants
    const i32 block_size;

    /// Pointer to next Block if this one is full (available <= 0)
    struct zl_BlockChunk* next;

    /// beginning of block data
    u8 storage[0];
} zl_BlockChunk;

#define zl_chunk_commit_offset(bc) (bc->storage + bc->committed)

/// @brief Virtual Memory Page metadata is stored on stack
/// with a small Bucket/Block/Chunk header
typedef struct {
    /// Root pointer to requested OS virtual memory page
    zl_BlockChunk* head;
    // zl_BlockChunk* back;

    /// Count of calls to zl_commit_memory
    i32 commits;

    /// Count of currently allocated (committed) bytes
    i32 commit_bytes;

    /// Byte sum of all commits that have taken place since this MemoryBlocks
    /// initialization
    i32 sum_commit_bytes;

    /// Count of calls to zl_decommit_memory
    i32 decommits;
    /// number of bytes that have been decommitted
    i32 sum_decommit_bytes;

    /// Count of calls to zl_reserve_memory
    i32 reserves;
    /// Number of bytes that are currently reserved
    i32 reserved_bytes;

    /// Byte sum of all commits that have taken place since this MemoryBlocks
    /// initialization
    i32 sum_reserved_bytes;

    /// Count of calls to zl_release_memory
    i32 releases;
    /// Number of bytes that have been reserved
    i32 sum_released_bytes;

    /// maximum number of children to nest when we need to grow/reserve more memory.
    /// no limit if set to -1. default is 0, thus growing is disabled and will
    /// either return an error or crash the program (i have yet to decide :D)
    const i32 max_children;
    i32 children_count;
    const zl_VMemType memtype;

} zl_MemoryBlock;

/// reinterpret cast page.head to u8* then offset by page.head.committed
#define zl_mblock_commit_offset(page) zl_chunk_commit_offset(page.head)

/// Same as zl_mempage_committed, but assumes pointer value
#define zl_pmblock_commit_offset(ptr) zl_chunk_commit_offset(ptr->head)

/// reinterpret cast zalloc_MemoryPage's head field from zalloc_PageHeader* to u8*
#define zl_mblock_data(page) ((u8*) (page.head))
/// Same as zl_mempage_data but assumes pointer value
#define zl_pmblock_data(ptr) ((u8*) (ptr->head))

/// Alias for mempage_data
#define zl_mblock_begin(page) zl_mblock_data(page)
/// Same as zl_mempage_begin but assumes pointer value
#define zl_pmblock_begin(ptr) zl_pmblock_data(ptr)

/// Gets pointer to end of memory page
/// @warning does not check header is valid
#define zl_mblock_end(page) (zl_mblock_begin(page) + page.head->size_bytes)
/// Same as zl_mempage_end but assumes pointer value
#define zl_pmblock_end(ptr) (zl_pmblock_begin(ptr) + ptr->head->size_bytes)

/// Creates a new MemoryBlock structure. Requests siez_bytes
/// from OS to reserve
/// @warning WARNING: The memory pointed to that is returned from this function
/// must not be read or written to until it is first committed!!! (use zl_mblock_grow
/// to commit!)
extern zl_MemoryBlock zl_mblock_new(const i64 size_bytes) ZEAL_NOEXCEPT;
/// Grows (Commits) mempage by size_bytes
extern int zl_mblock_grow(zl_MemoryBlock* page, const i64 size_bytes) ZEAL_NOEXCEPT;

/// Shrinks (De-Commits) mempage by size_bytes
extern int zl_mblock_shrink(zl_MemoryBlock* page,
                            const i64 size_bytes) ZEAL_NOEXCEPT;
/// Deletes (Releases) mempage
extern int zl_mblock_delete(zl_MemoryBlock* page) ZEAL_NOEXCEPT;

ZEAL_CAPI_END

ZEAL_NAMESPACE(alloc)

#ifdef __cplusplus

namespace os {
inline u64 get_page_size() noexcept {
    return zl_get_page_size();
}

template <typename T>
inline T* memory_reserve(const i64 size_bytes) noexcept {
    void* mem = zl_reserve_memory(size_bytes);
    return static_cast<T*>(mem);
}

template <typename T>
inline void memory_commit(T* memory, const i64 size_bytes) noexcept {
    zl_commit_memory(memory, size_bytes);
}

template <typename T>
inline void memory_decommit(T* memory, const i64 size_bytes) noexcept {
    zl_decommit_memory(memory, size_bytes);
}

template <typename T>
inline void memory_release(T* memory, const i64 size_bytes) noexcept {
    zl_release_memory(memory, size_bytes);
}
}  // namespace os

#endif

ZEAL_NAMESPACE_END

#endif
