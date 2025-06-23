#include "spool.h"

#include <cstring>

#include "mem.h"
#include "plog/Log.h"

zl_StringPool zl_stringpool_new(const i32 size_bytes) noexcept {
    zl_MemoryBlock mb = zl_mblock_new(1, 0);
    zl_mblock_extend_bytes(&mb, size_bytes);
    return {
        .memory = mb,
        .len_bytes = 0,
        .count = 0,
    };
}

void zl_stringpool_reset(zl_StringPool* self, const bool zeroise) noexcept {
    self->len_bytes = 0;
    self->count = 0;
    if (zeroise) {
        u8* begin = self->memory.begin;

        // set all committed memory to 0
        std::memset(begin, 0, self->memory.committed);
    }
}

const char* zl_stringpool_push(zl_StringPool* self, const char* str) noexcept {
    const auto len = std::strlen(str);
    return zl_stringpool_pushlen(self, str, len);
}

const char* zl_stringpool_pushlen(zl_StringPool* self, const char* str,
                                  const i32 stlen) noexcept {
    if (!self || !str || stlen <= 0) return nullptr;

    const i32 len = self->len_bytes;
    const i32 next_top = len + stlen;

    // Check if we have to commit first before writing
    if (self->memory.committed <= next_top) {
        // commit a KB if we have 0 len (this is the first push/commit)
        const i32 pbytes = (len <= 0) ? (1024) : (len * 2);
        if (const auto err = zl_mblock_extend_bytes(&self->memory, pbytes);
            err != 0) {
            PLOGE << "Error occurred while pushing bytes to memory block!";
            return nullptr;
        }
    }

    // copy string to our pool
    u8* top = self->memory.begin + self->len_bytes;
    std::memcpy(top, str, stlen);

    // cache begining of string after write
    char* res = reinterpret_cast<char*>(top);

    // write null terminator to end of string
    top += stlen;
    *top = '\0';

    // update fields
    self->len_bytes += stlen;
    self->count += 1;

    return res;
}

void zl_stringpool_free(zl_StringPool* self) noexcept {
    if (!self) return;
    zl_mblock_free(&self->memory);
}
