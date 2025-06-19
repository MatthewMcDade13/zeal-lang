#ifndef _ZEAL_ALLOC_INCL_MEMORY_H_
#define _ZEAL_ALLOC_INCL_MEMORY_H_

#include <cstddef>

#include "common.h"
// #include "common.h"

ZEAL_CAPI_BEGIN

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
extern void zl_commit_memory(void* memory, const i64 size_bytes) ZEAL_NOEXCEPT;

/// @breif Decommits pages
/// @details Decommits range of pages, releasing their physical memory.
/// @warning The address space at @param memory remains reserved, but becomes
/// inaccessible to read/writes
extern void zl_decommit_memory(void* memory, const i64 size_bytes) ZEAL_NOEXCEPT;

/// @breif Releases entire block of virutal memory back to operating system
extern void zl_release_memory(void* memory, const i64 size_bytes) ZEAL_NOEXCEPT;

typedef struct zl_BlockHeader {
    /// size of entire reserved virtual memory in bytes,
    /// this includes the sizeof the header itself.
    /// comparatively, the size_bytes field in zl_MemoryBlock is
    /// the USER DATA size, which is sizeof(zl_BlockHeader).less bytes than this
    /// fields value
    const i32 size_bytes;

    /// Is all the memory in this block taken?
    bool isfull;

    /// Pointer to next Block if this one is full (available <= 0)
    struct zl_BlockHeader* next;

    /// beginning of block data
    u8 storage[0];
} zl_BlockHeader;

/// @breif OS Virtual Memory Page
/// @remarks used to track how memory is committed/decommitted
typedef struct {
    /// Root pointer to requested OS virtual memory page
    zl_BlockHeader* head;
    /// count of bytes committed so far.
    /// This considers the full size of the block, so its
    /// normal for committed != size_bytes when block is full
    i32 committed;
    /// Size of the available memory in bytes
    /// @warning this does not consider the size of the headers,
    /// so its sizeof(zl_BlockHeader) bytes LESS than the actual full
    /// reserved memory space, whos value would be found in the header alongsize the
    /// pointer to the acutal memory
    i32 size_bytes;
} zl_MemoryBlock;

/// reinterpret cast page.head to u8* then offset by page.committed
#define zl_mblock_committed(page) (((u8*) (page.head)) + page.committed)
/// Same as zl_mempage_committed, but assumes pointer value
#define zl_pmblock_committed(ptr) (((u8*) (ptr->head)) + ptr->committed)

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
extern void zl_mblock_grow(zl_MemoryBlock* page, const i64 size_bytes) ZEAL_NOEXCEPT;
/// Shrinks (De-Commits) mempage by size_bytes
extern void zl_mblock_shrink(zl_MemoryBlock* page,
                             const i64 size_bytes) ZEAL_NOEXCEPT;
/// Deletes (Releases) mempage
extern void zl_mblock_delete(zl_MemoryBlock* page) ZEAL_NOEXCEPT;

/// Walks the list of BlockHeaders and gets the first non-full chunk of memory
extern u8* zl_mblock_next_avail(zl_MemoryBlock* block) ZEAL_NOEXCEPT;

/// A user-provided callback function for iterating over memory Blocks
/// receives a pointer to current iterated chunk, and the number of bytes
/// that is currently committed in that block. committed_bytes will always be -1 if
/// the current chunk is full. you can also cast the passed in pointer to
/// zl_BlockHeader to get the full size of the block
typedef void (*zl_ChunkIter)(u8* chunk, const i32 commited_bytes);
/// Walks the list of BlockHeaders and passes the head of the memory chunk along with
/// the count of committed bytes in the memory chunk to the user-provided callback
extern void zl_mblock_foreach(zl_MemoryBlock* block,
                              zl_ChunkIter callback) ZEAL_NOEXCEPT;

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
