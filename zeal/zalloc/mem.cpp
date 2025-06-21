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
    if (!mblock) return -1;
    const auto size_bytes = PAGESIZE * (count <= 0 ? 1 : count);
    return zl_mblock_push_bytes(mblock, size_bytes);
}

/// Grows (Commits) mblock by size_bytes
i32 zl_mblock_push_bytes(zl_MemoryBlock* mblock, const i64 grow_bytes) noexcept {
    if (!mblock) return zl_VMemErrorType__InvalidArgs;
    if (!mblock->begin) return zl_VMemErrorType__InvalidArgs;
    if (mblock->committed >= mblock->capacity)
        return zl_VMemErrorType__OutOfReservedMemory;

    u8* commit_top = mblock->begin + mblock->committed;
    u64 size = grow_bytes < 0 ? 0 : static_cast<u64>(grow_bytes);

    const u8* next_top = commit_top + size;

    if (next_top >= mblock->end) {
        // We overshot it, so try to at least commit the rest
        // of reserved memory, so update size to the delta of
        // committed and chunk_size
        size = mblock->capacity - mblock->committed;

        if (size == 0) {
            return zl_VMemErrorType__OutOfReservedMemory;
        } else {
            // capacity was somehow less than committed. Possible
            // data corruption or sneaky bug, bail out!
            return zl_VMemErrorType__InvalidArgs;
        }
    }

    if (const i32 err = zl_vmemory_commit(commit_top, size); err != 0) {
        Zeal_Panic("Failed to commit %d byte subregion of reserved memory ",
                   static_cast<i32>(size));
        return {};
    }

    mblock->commits += 1;
    mblock->committed += size;
    mblock->sum_commit_bytes += size;

    return zl_VMemErrorType__Ok;
}

/// Deletes (Releases) mblock
i32 zl_mblock_delete(zl_MemoryBlock* mblock) noexcept {
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
