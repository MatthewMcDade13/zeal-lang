#include "incl/mem.h"

#include <cstdio>
#include <cstring>

#include "common.h"
#include "plog/Log.h"

#ifdef _WIN32
#define WINDOWS 1
#define UNIX 0
#endif

#if defined(__linux__) || defined(__APPLE__)
#define WINDOWS 0
#define UNIX 1
#endif

#if WINDOWS == 0 && UNIX == 0
#error "Platform not supported!!"
#endif

#if WINDOWS
#include <windows.h>
#elif UNIX
#include <sys/mman.h>
#include <unistd.h>
#endif

// Platform specific helper functions
// these all check for errors and return error code of error that occurred, if any.
// these also log when error happens with plog
#if WINDOWS
static void* virtual_alloc(const i64 size_bytes, const i32 memflags,
                           const i32 pageflags);
static i32 win32_commit(void* memory, const i64 size_bytes);
static i32 win32_decommit(void* memory, const i64 size_bytes);
static i32 win32_release(void* memory, const i64 size_bytes);
#endif
#if UNIX
static void* unix_mmap(const i64 size_bytes, i32 prot, i32 flags) noexcept;
static i32 unix_commit(void* memory, const i64 size_bytes) noexcept;
static i32 unix_decommit(void* memory, const i64 size_bytes) noexcept;
static i32 unix_release(void* memory, const i64 size_bytes) noexcept;
#endif

/// Wrapper funcitons to tidy up duplication of #if WINDOWS #elif UNIX #else #endif
/// chains
static void* mem_reserve(const i64 size_bytes) noexcept;
static i32 mem_commit(void* memory, const i64 size_bytes) noexcept;
static i32 mem_decommit(void* memory, const i64 size_bytes) noexcept;
static i32 mem_release(void* memory, const i64 size_bytes) noexcept;

static i32 mblock_header_init(zl_MemoryBlock* block, u8* memory,
                              const i32 size_bytes) noexcept;

static i32 mblock_append(zl_MemoryBlock* block, const i32 size_bytes) noexcept;

// ====================================
// Implementation of C header functions
// ====================================

i64 zl_get_page_size(void) noexcept {
#if WINDOWS
    SYSTEM_INFO sysinfo{};
    GetSystemInfo(&sysinfo);
    static const auto page_size = sysinfo.dwPageSize;
    return page_size;
#elif UNIX
    static const auto page_size = sysconf(_SC_PAGESIZE);
    return page_size;
#else
#error \
    "Unsupported platform. zalloc_get_page_size will ALWAYS return 0 while on unsupprted platform!!!! Supported Platforms: _WIN32, __linux__, __APPLE__"
    return 0;
#endif
}
//
// #if ZEAL_DEBUG
// #define CHECK(page) \
//     do { \
//         if (!page.head) { \
//             PLOGE << "zalloc_MemoryPage head field is nullptr! aborting!"; \
//             assert(false); \
//             \                                                                     \
//         } \
//         if (page.size_bytes <= 0) { \
//         } \
//         assert(page.head != nullptr && page.size_bytes > 0); \
//         const auto head = *page->head; \
//         if (head.canary != CANARY) { \
//             PLOGE << "Allocated Memory page is corrupt! Canary value has been " \
//                      "changed!! found: " \
//                   << std::to_string(head.canary); \
//             assert(false); \
//         } \
//         if (page.committed != head.committed) { \
//             PLOGE << "local committed value is different from head.committed " \
//                      "value!"; \
//             assert(false); \
//         } \
//         if (page.size_bytes != page.head->size_bytes) { \
//             PLOGE << "local size_bytes value is different from head->size_bytes "
//             \
//                      "value!"; \
//             assert(false); \
//         } \
//     } while (0);
//
// #else
// #define CHECK(page)  \
//     do {             \
//         (void) page; \
//     } while (0);
// #endif
//
void* zl_reserve_memory(const i64 size_bytes) noexcept {
    static const i64 page_size = zl_get_page_size();
    if (size_bytes < page_size) {
        PLOGE << "Request to reserve memory of size: " << std::to_string(size_bytes)
              << " bytes failed as it is less than OS page size: "
              << std::to_string(page_size);
        return nullptr;
    }
    return mem_reserve(size_bytes);
}

i32 zl_commit_memory(void* memory, const i64 size_bytes) noexcept {
    return mem_commit(memory, size_bytes);
}

i32 zl_decommit_memory(void* memory, const i64 size_bytes) noexcept {
    return mem_decommit(memory, size_bytes);
}

i32 zl_release_memory(void* memory, const i64 size_bytes) noexcept {
    return mem_release(memory, size_bytes);
}

zl_MemoryBlock zl_mblock_new(const i64 size_bytes) noexcept {
    u8* memory = static_cast<u8*>(zl_reserve_memory(size_bytes));
    // zl_commit_memory(memory, sizeof(zl_BlockHeader) + size_bytes / 4);
    if (memory) {
        return {
            // WARNING: This memory is uninitialized, so don't read from it until
            // the first commit is made!
            .head = reinterpret_cast<zl_BlockChunk*>(memory),
            // .committed = 0,
            // .size_bytes = static_cast<i32>(size_bytes),
        };
    }
    Zeal_Panic("%s", "Failed to reserve memory!");
    return {};
}

u8* zl_mblock_next_avail(zl_MemoryBlock* block) noexcept {
    // TODO: IMPLEMENT ME
    PLOGF << "TOOD: IMPLEMENT ME!";
    Zeal_Panic("%s", "Panic! in the Runtime!");
    return nullptr;
    // while (iter) {
    //     last = iter;
    //     iter = iter->next;
    // }
    // do {
    //     last = last->next;
    // } while (nullptr != last);
    // for (zl_BlockHeader* iter = page->head->next; nullptr != iter;
    //      iter = iter->next) {
    //     last = iter;
    // }
    // zl_BlockHeader* avail = page->head->next;
    // while (nullptr != avail) {
    //     avail = avail->next;
    // }
}

static zl_BlockChunk* mblock_leaf(const zl_MemoryBlock& block) noexcept {
    if (!block.head || block.bytes_committed == 0) {
        PLOGE << "Attempt to use un-committed memory!";
        return nullptr;
    }
    zl_BlockChunk* head = block.head;
    /// created with no intention of appending children, so
    /// block.head is it!
    if (!head->next || block.max_children == 0) {
        return head;
    }

    zl_BlockChunk* iter = head->next;
    while (iter->next) {
        iter = iter->next;
    }
    return iter;
}

/// Grows (Commits) mblock by size_bytes
i32 zl_mblock_grow(zl_MemoryBlock* page, const i64 size_bytes) noexcept {
    if (!page) {
        return;
    }
    if (!page->head) {
        return;
    }

    // ensure we are within reserved bounds to allow
    // for a commit
    u8* committed = zl_pmblock_commit_offset(page);
    const u8* end = zl_pmblock_end(page);

    if ((committed + size_bytes) >= end) {
        /// if there is no next block, grow!
        if (!page->head->next) {
            page->head->isfull = true;
            const auto size = page->head->block_size * 2;
            u8* child = static_cast<u8*>(zl_reserve_memory(size));
            zl_commit_memory(child, (size_bytes + sizeof(zl_BlockChunk)) * 2);

            zl_BlockChunk h{.block_size = size, .isfull = false, .next = nullptr};
            memcpy(child, &h, sizeof(zl_BlockChunk));
            page->head->next = reinterpret_cast<zl_BlockChunk*>(child);
        } else {
            // otherwise get the next available block...

            auto* next = zl_mblock_next_avail(page);
            if (next == zl_pmblock_data(page) || next == nullptr) {
                Zeal_Panic("%s",
                           "Failed to grow MemoryBlock for unknown reasons. "
                           "Probably logic error!!!");
                return;
            }
        }

        // Zeal_Panic("%s", "Attempt to grow past current committed virtual
        // memory!");
        return;
    }

    zl_commit_memory(committed, size_bytes);
    auto* head = page->head;

    page->committed += size_bytes;
}

/// Shrinks (De-Commits) mblock by size_bytes
void zl_mblock_shrink(zl_MemoryBlock* page, const i64 size_bytes) {
    if (!page) {
        return;
    }
    const auto p = *page;
    CHECK(p);
    u8* commit_mem_old = zl_mblock_committed(p);
    u8* commit_mem = commit_mem_old - size_bytes;

    // Handle underflow, otherwise just decommit as normal
    if (commit_mem <= zl_mblock_begin(p)) {
        const auto size = commit_mem_old - zl_mblock_begin(p);
        zl_decommit_memory(page->head, size);
        page->committed = 0;
        page->head->committed = 0;
    } else {
        zl_decommit_memory(commit_mem, size_bytes);
        page->head->committed -= size_bytes;
        page->committed = page->head->committed;
    }
}

/// Deletes (Releases) mblock
void zl_mblock_delete(zl_MemoryBlock* page) {
    if (!page) {
        return;
    }
    const auto p = *page;
    CHECK(p);

    void* memory = reinterpret_cast<void*>(p.head);
    if (memory) {
        zl_release_memory(memory, p.size_bytes);
        memset(page, 0, sizeof(zl_MemoryBlock));
    }
}

// ========================
// Utility function impls
// ========================

#if WINDOWS
i32 win32_commit(void* memory, const i64 size_bytes) {
    auto* res = VirtualAlloc(memory, size_bytes, MEM_COMMIT, PAGE_READWRITE);
    if (!res) {
        const DWORD errcode = GetLastError();
        PLOGE << "Failed to commit memory! Call to VirtualAlloc failed! Error Code: "
              << std::to_string(errcode);
        return errcode;
    }
    return 0;
}
void* virtual_alloc(const i64 size_bytes, const i32 memflags, const i32 pageflags) {
#if WINDOWS
    void* memory = VirtualAlloc(0, size_bytes, MEM_RESERVE, PAGE_NOACCESS);
    if (!memory) {
        const DWORD errcode = GetLastError();
        PLOGE << "Call to VirtualAlloc(0, " << std::to_string(size_bytes)
              << ", MEM_RESERVE, PAGE_NOACCESS) Failed! Error Code: "
              << std::to_string(errcode);
        return nullptr;
    }
    return memory;
#else
    return nullptr;
#endif
}
i32 win32_decommit(void* memory, const i64 size_bytes) {
    if (!VirtualFree(memory, size_bytes, MEM_DECOMMIT)) {
        const DWORD err = GetLastError();

        PLOGE
            << "Call to VirtualFree Failed!! could not decommit memory! Error Code: "
            << std::to_string(err);
        return err;
    }
    return 0;
}

i32 win32_release(void* memory, const i64 size_bytes) {
    if (!VirtualFree(memory, size_bytes, MEM_RELEASE)) {
        const DWORD err = GetLastError();

        PLOGE
            << "Call to VirtualFree Failed!! Could not release memory! Error Code: "
            << std::to_string(err);
        return err;
    }
    return 0;
}
#endif

#if UNIX
i32 unix_commit(void* memory, const i64 size_bytes) noexcept {
    i32 err = mprotect(memory, size_bytes, PROT_READ | PROT_WRITE);
    if (err == -1) {
        const i32 err = errno;
        PLOGE << "Failed call to mprotect! errno: " << std::to_string(err);
        return err;
    }
    return 0;
}

void* unix_mmap(const i64 size_bytes, i32 prot, i32 flags) noexcept {
    void* memory = mmap(nullptr, size_bytes, prot, flags, -1, 0);
    if (memory == MAP_FAILED) {
        PLOGE << "Call to mmap(nullptr, " << std::to_string(size_bytes)
              << ", PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0) Failed! Error "
                 "Code: "
              << std::to_string(errno);
        return nullptr;
    }
    return memory;
}

i32 unix_decommit(void* memory, const i64 size_bytes) noexcept {
    i32 err = madvise(memory, size_bytes, MADV_DONTNEED);
    if (err != 0) {
        const i32 err = errno;
        PLOGE << "Could not decommit memory! Call to madvice failed! errno: "
              << std::to_string(err);
        return err;
    }
    return 0;
}
i32 unix_release(void* memory, const i64 size_bytes) noexcept {
    i32 err = munmap(memory, size_bytes);
    if (err != 0) {
        const i32 err = errno;
        PLOGE << "Could not release memory! Call to munmap failed! errno: "
              << std::to_string(err);
        return err;
    }
    return 0;
}
#endif

void* mem_reserve(const i64 size_bytes) noexcept {
#if WINDOWS
    return virtual_alloc(size_bytes, MEM_RESERVE, PAGE_NOACCESS);
#elif UNIX
    return unix_mmap(size_bytes, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS);
#else
#error \
    "Unsupported platform. zalloc_reserve_memory will ALWAYS return nullptr/NULL while on unsupprted platform!!!! Supported Platforms: _WIN32, __linux__, __APPLE__"
    return nullptr;
#endif
}

int mem_commit(void* memory, const i64 size_bytes) noexcept {
#if WINDOWS
    return win32_commit(memory, size_bytes);
#elif UNIX
    return unix_commit(memory, size_bytes);
#else
    (void) memory;
    (void) size_bytes;
    return ();
#endif
}
void mem_decommit(void* memory, const i64 size_bytes) noexcept {
#if WINDOWS
    return win32_decommit(memory, size_bytes);
#elif UNIX
    return unix_decommit(memory, size_bytes);
#else

#error \
    "Unsupported platform. zalloc_decommit_memory will ALWAYS be a no-op while on unsupprted platform!!!! Supported Platforms: _WIN32, __linux__, __APPLE__"
    return ();
#endif
}
void mem_release(void* memory, const i64 size_bytes) noexcept {
#if WINDOWS
    return win32_release(memory, size_bytes);
#elif UNIX
    return unix_release(memory, size_bytes);
#else

#error \
    "Unsupported platform. zalloc_release_memory will ALWAYS be a no-op while on unsupprted platform!!!! Supported Platforms: _WIN32, __linux__, __APPLE__"
    return ();
#endif
}
void mblock_header_init(zl_MemoryBlock* block, u8* __restrict memory,
                        const i32 size_bytes) noexcept {
    if (!block) {
        PLOGE << "Attempt to operate on a nullptr!";
        return;
    }

    if (!block->head) {
        PLOGE << "Attempt to operate on a nullptr! MemoryBlock Header  field is "
                 "nullptr!!";
        return;
    }

    constexpr const int HEAD_SIZE = sizeof(zl_BlockChunk);
    if (block->committed < HEAD_SIZE) {
        Zeal_Panic(
            "MemoryBlock must have committed memory larger than %d, "
            "(sizeof(zl_BlockHeader)) in bytes. Current committed memory in bytes "
            "is: %d\n",
            HEAD_SIZE, block->committed);
        return;
    }
    const zl_BlockChunk head{
        .block_size = size_bytes, .isfull = false, .next = nullptr};
    memcpy(memory, &head, sizeof(zl_BlockChunk));

    block->head = reinterpret_cast<zl_BlockChunk*>(memory);
}

void mblock_append(zl_MemoryBlock* block, const i32 size_bytes) noexcept {
    if (!block) return;
    if (!block->head) return;

    zl_BlockChunk* last = block->head;
    while (last->next) {
        last = last->next;
    }

    while (iter) {
    }
}
