#include "incl/mem.h"

#include <cstdio>
#include <cstring>
#include <iostream>

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
static inline char ZEAL_MBLOCK_STATS_TEMPL_PRETTY[] =
    "{\n"
    "\t\"commits\": %d,"
    "\t\"committed_bytes\": %d,"
    "\t\"sum_committed_bytes\": %d,"
    "\t\"decommits\": %d,"
    "\t\"sum_decommit_bytes\": %d,"
    "\t\"available_bytes\": %d,"
    "\t\"capacity\": %d,"
    "}\n";

static inline char ZEAL_MBLOCK_STATS_TEMPL[] =
    "{"
    "\"commits\": %d,"
    "\"committed_bytes\": %d,"
    "\"sum_committed_bytes\": %d,"
    "\"decommits\": %d,"
    "\"sum_decommit_bytes\": %d,"
    "\"available_bytes\": %d,"
    "\"capacity\": %d,"
    "}";

void zl_mblock_stats_string(const zl_MemoryBlock* mblock, char* buf, const i32 len,
                            const bool pretty) ZEAL_NOEXCEPT {
    if (pretty) {
        sprintf(buf, ZEAL_MBLOCK_STATS_TEMPL_PRETTY, mblock->commits,
                mblock->committed, mblock->sum_commit_bytes, mblock->decommits,
                mblock->sum_decommit_bytes, mblock->available, mblock->capacity);
    } else {
        sprintf(buf, ZEAL_MBLOCK_STATS_TEMPL, mblock->commits, mblock->committed,
                mblock->sum_commit_bytes, mblock->decommits,
                mblock->sum_decommit_bytes, mblock->available, mblock->capacity);
    }
}

void zl_mblock_print_stats(const zl_MemoryBlock* mblock) ZEAL_NOEXCEPT {
    char json[255]{};
    zl_mblock_stats_string(mblock, json, 255, true);
    json[254] = '\0';
    std::cout << json << "\n";
}

// ====================================
// Implementation of C header functions
// ====================================

static const auto PAGESIZE = zl_get_page_size();

u64 zl_get_page_size(void) noexcept {
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

void* zl_vmemory_reserve_bytes(const i64 size_bytes) noexcept {
    static const i64 page_size = zl_get_page_size();
    if (size_bytes < page_size) {
        PLOGE << "Request to reserve memory of size: " << std::to_string(size_bytes)
              << " bytes failed as it is less than OS page size: "
              << std::to_string(page_size);
        return nullptr;
    }
    return mem_reserve(size_bytes);
}

i32 zl_vmemory_commit(void* memory, const i64 size_bytes) noexcept {
    return mem_commit(memory, size_bytes);
}

i32 zl_vmemory_decommit(void* memory, const i64 size_bytes) noexcept {
    return mem_decommit(memory, size_bytes);
}

i32 zl_vmemory_free(void* memory, const i64 size_bytes) noexcept {
    return mem_release(memory, size_bytes);
}
zl_MemoryBlock zl_mblock_new(const i32 page_count, const i32 commit_pages) noexcept {
    constexpr const auto PAGESIZE = VMEMORY_4KB;
    assert(page_count >= 1);

    const auto size_bytes = PAGESIZE * page_count;

    u8* memory = static_cast<u8*>(zl_vmemory_reserve_bytes(size_bytes));
    if (!memory) {
        Zeal_Panic("%s: %d %s", "Failed to reserve", static_cast<i32>(size_bytes),
                   "bytes of virtual memory from system");
        return {};
    }

    zl_MemoryBlock mblock{};
    mblock.begin = memory;
    mblock.end = memory + size_bytes;
    mblock.capacity = size_bytes;
    mblock.available = size_bytes;

    if (commit_pages != 0) {
        i32 cpages = 0;
        /// full commit if we got -1
        if (commit_pages <= -1) {
            cpages = page_count;
        } else {
            cpages = std::min(commit_pages, page_count);
        }
        if (const i32 err = zl_mblock_push_pages(&mblock, cpages); err != 0) {
            Zeal_Panic(
                "Error occured while committing pages in zl_MemoryBlock "
                "Constructor Function! Zeal Error Code: %d",
                err);
            return {};
        }
    }
    return mblock;
}

i32 zl_mblock_push_pages(zl_MemoryBlock* mblock, const i32 count) noexcept {
    const auto size_bytes = PAGESIZE * (count <= 0 ? 1 : count);
    return zl_mblock_push_bytes(mblock, size_bytes);
}

/// Grows (Commits) mblock by size_bytes
i32 zl_mblock_push_bytes(zl_MemoryBlock* mblock, const i64 grow_bytes) noexcept {
    if (!mblock || grow_bytes < 0) return zl_VMemErrorType__InvalidArgs;
    if (!mblock->begin) return zl_VMemErrorType__InvalidArgs;
    if (mblock->committed >= mblock->capacity)
        return zl_VMemErrorType__OutOfReservedMemory;

    u8* top = mblock->begin + mblock->committed;

    // Clamp to available in case of overflow
    const auto size = std::min(static_cast<i32>(grow_bytes), mblock->available);
    assert(top + size <= mblock->end);

    if (const i32 err = zl_vmemory_commit(top, size); err != 0) {
        Zeal_Panic("Failed to commit %d byte subregion of reserved memory ",
                   static_cast<i32>(size));
        return {};
    }

    mblock->commits += 1;
    mblock->committed += size;
    mblock->sum_commit_bytes += size;
    mblock->available -= size;

    return zl_VMemErrorType__Ok;
}

/// Deletes (Releases) mblock
i32 zl_mblock_free(zl_MemoryBlock* mblock) noexcept {
    if (!mblock) return zl_VMemErrorType__InvalidArgs;
    if (!mblock->begin) return zl_VMemErrorType__InvalidArgs;

    void* memory = reinterpret_cast<void*>(mblock->begin);

    if (const i32 err = zl_vmemory_free(memory, mblock->capacity); err != 0) {
        Zeal_Panic("%s. Zeal Error Code: %d",
                   "Error occurred while attempting to free MemoryBlock", err);
        return zl_VMemErrorType__ReleaseFail;
    }
    memset(mblock, 0, sizeof(zl_MemoryBlock));
    return zl_VMemErrorType__Ok;
}

/// commits all reserved memory
i32 zl_mblock_full_commit(zl_MemoryBlock* memory) ZEAL_NOEXCEPT {
    return zl_mblock_push_bytes(memory, memory->available);
}

/// Shrinks (De-Commits) mempage by size_bytes
i32 zl_mblock_pop_bytes(zl_MemoryBlock* mblock, const i64 size_bytes) ZEAL_NOEXCEPT {
    if (!mblock) return zl_VMemErrorType__InvalidArgs;
    if (!mblock->begin || size_bytes <= 0) return zl_VMemErrorType__InvalidArgs;

    u8* top = mblock->begin + mblock->committed;
    const auto size = std::min(static_cast<i32>(size_bytes), mblock->available);
    assert(top + size <= mblock->end);

    if (const i32 err = zl_vmemory_decommit(top, size); err != 0) {
        Zeal_Panic("Failed to decommit %d byte subregion of reserved memory ",
                   static_cast<i32>(size));
        return err;
    }

    mblock->commits -= 1;
    mblock->committed -= size;
    mblock->sum_decommit_bytes += size;
    mblock->available += size;
    return zl_VMemErrorType__Ok;
}

/// Pops (Decommits) page size * count
i32 zl_mblock_pop_pages(zl_MemoryBlock* memory, const i32 count) ZEAL_NOEXCEPT {
    return zl_mblock_pop_bytes(memory, PAGESIZE * count);
}
// ========================
// Utility function impls
// ========================

#if WINDOWS
i32 win32_commit(void* memory, const i64 size_bytes) {
    auto* res = VirtualAlloc(memory, size_bytes, MEM_COMMIT, PAGE_READWRITE);
    if (!res) {
        const DWORD errcode = GetLastError();
        PLOGE << "Failed to commit memory! Call to VirtualAlloc failed! Error "
                 "Code: "
              << std::to_string(errcode);
        return errcode;
    }
    return 0;
}
void* virtual_alloc(const i64 size_bytes, const i32 memflags, const i32 pageflags) {
    void* memory = VirtualAlloc(0, size_bytes, MEM_RESERVE, PAGE_NOACCESS);
    if (!memory) {
        const DWORD errcode = GetLastError();
        PLOGE << "Call to VirtualAlloc(0, " << std::to_string(size_bytes)
              << ", MEM_RESERVE, PAGE_NOACCESS) Failed! Error Code: "
              << std::to_string(errcode);
        return nullptr;
    }
    return memory;
}
i32 win32_decommit(void* memory, const i64 size_bytes) {
    if (!VirtualFree(memory, size_bytes, MEM_DECOMMIT)) {
        const DWORD err = GetLastError();

        PLOGE << "Call to VirtualFree Failed!! could not decommit memory! Error "
                 "Code: "
              << std::to_string(err);
        return err;
    }
    return 0;
}

i32 win32_release(void* memory, const i64 size_bytes) {
    if (!VirtualFree(memory, size_bytes, MEM_RELEASE)) {
        const DWORD err = GetLastError();

        PLOGE << "Call to VirtualFree Failed!! Could not release memory! Error "
                 "Code: "
              << std::to_string(err);
        return err;
    }
    return 0;
}
#endif

#if UNIX
i32 unix_commit(void* memory, const i64 size_bytes) noexcept {
    int flags = PROT_READ | PROT_WRITE;
    i32 err = mprotect(memory, size_bytes, flags);
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
i32 mem_decommit(void* memory, const i64 size_bytes) noexcept {
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
i32 mem_release(void* memory, const i64 size_bytes) noexcept {
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
