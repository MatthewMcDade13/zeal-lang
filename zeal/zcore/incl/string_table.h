#pragma once
#include "common.h"

#include <plog/Log.h>

namespace zeal::core {

/// A collection of unique strings (separated by a single whitespace and/or a null
/// terminator \0 in memory) a small unordered flat set is used to track strings that
/// are already in table. Runes are distinguised from symbols by their ':' prefix
//
/// NOTE: This table is used for general string interning, so compile time and static
/// constant strings will also reside here
///
/// @warning std::string_view 's returned from lookup and insertion are only valid
/// the lifetime of this Table. and for that reason this table is not copyable
struct StringTable final {
    struct Handle {
        const usize id;
        const usize len;

        constexpr Handle(const StringTable& parent_in, const usize id_in,
                         const usize len_in) noexcept
            : id(id_in), len(len_in), parent(parent_in) {}

        static constexpr Handle none(const StringTable& parent) noexcept {
            return Handle(parent, 0, 0);
        }

        constexpr Str get() const noexcept {
            if (const auto opt = this->parent.get(*this)) {
                return opt.value();
            }
            return {};
        }

        constexpr bool is_none() const noexcept {
            return this->id == 0 && this->len == 0;
        }

        constexpr operator bool() const noexcept { return !this->is_none(); }

       private:
        const StringTable& parent;
    };

    static constexpr auto STATIC_CAP = 128;
    constexpr StringTable() = default;

    template <const usize CAP = STATIC_CAP>
    static constexpr StringTable with_capacity() noexcept {
        StringTable st{};

        constexpr const auto BUFFER_SIZE = CAP * 24;
        // ensure below reserve call wont throw.
        static_assert(BUFFER_SIZE <= st.symbols.max_size(), "Capacity too large!");

        // but lets put these in a try/catch just in case... (i hate exceptions
        // btw ugh... lol)
        try {
            st.symbols.reserve(BUFFER_SIZE);
        } catch (const std::exception& e) {
            // log exception, and return default constructed SymbolTable.
            PLOGE << "Error reserving capacity for SymbolTable!\n\t=> " << e.what();
        }

        return st;
    }

    /// Adds symbol to symbol table. Returns handle to added symbol, or handle to
    /// existing symbol if given symbol is already in table
    Handle add(const Str symbol) noexcept;
    /// Linearly scans through buffer for @param symbol.
    Opt<Str> lookup_str(const Str symbol) const noexcept;
    /// @brief  Linearly scans through buffer for @param symol
    /// @returns std::optional<Handle> if found, std::nullopt otherwise
    Opt<Handle> lookup_handle(const Str symbol) const noexcept;
    /// Indexes internal string table to get a view that points to requested symbol
    Opt<Str> get(const Handle handle) const noexcept;

    /// Same as lookup_str, but also looks for @param ':' + name as well
    Opt<Str> lookup_or_rune_str(const Str name) const noexcept;

    /// Same as lookup_handle, but also looks for @param ':' + name if not found with
    /// @param name alone
    Opt<Handle> lookup_or_rune_handle(const Str symbol) const noexcept;

    constexpr bool has(const Str name) const noexcept {
        return this->lookup_str(name).has_value();
    }

    StringTable(StringTable&&) noexcept = default;
    StringTable& operator=(StringTable&&) noexcept = default;

   private:
    StringTable(const StringTable&) = delete;
    StringTable& operator=(const StringTable&) = delete;

    Handle append(const Str name);

    Str substr(const usize id, const usize len) const noexcept;
    constexpr Str substr(const Handle handle) const noexcept {
        return this->substr(handle.id, handle.len);
    }

    String symbols{};
};

}  // namespace zeal::core
