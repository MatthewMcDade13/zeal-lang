#pragma once
#include <plog/Log.h>

#include "common.h"

// namespace zeal::core {

// /// A collection of unique strings (separated by a single whitespace and/or a null
// /// terminator \0 in memory) a small unordered flat set is used to track strings that
// /// are already in table. Runes are distinguised from symbols by their ':' prefix
// //
// /// NOTE: This table is used for general string interning, so compile time and static
// /// constant strings will also reside here
// ///
// /// @warning std::string_view 's returned from lookup and insertion are only valid
// /// the lifetime of this Table. and for that reason this table is not copyable
// struct StringTable final {
//     static constexpr auto RESERVE_BYTES = 4096;
//     constexpr StringTable() = default;

//     struct Handle {
//         // NOTE: Might want to eventually do something like this where we store the
//         // type of internned string in some way. This could simplify lookups and
//         // comparisons potentially for now we can just check first character to tell
//         // if the interned string is an atom,symbol or string with prefixes ':' for
//         // atoms/runes, '"' for strings, or symbols if not prefixed at all
//         //
//         //
//         // enum class Type : u8 {
//         //     Empty = 0,
//         //     /// An unprefixed, unquoted string of characters
//         //     /// Normally this will AST artifacts that don't fall into
//         //     /// any of the other variants, like variable names, ect
//         //     Symbol,
//         //     /// Symbol prefixed with ':', ex: :atom or :"long atom" whos
//         //     /// value is always itself and contains only lowercase alphanumeric
//         //     /// characters, whitespace (only when usd with double quotes), or '_'
//         //     Atom,
//         //     /// Symbol surrounded by double quotes, ex: "this is a string"
//         //     String,
//         //     /// A special kind of Atom whos first character is always [A-Z]
//         //     /// and the rest of its letters are only valid alphanumeric
//         //     characters.
//         //     /// ex: CamelCaseForTypeNamesPlease
//         //     Type,
//         //     /// A regular atom that corresponds to a function
//         //     /// This helps to quickly distinguish what kind of internned string we
//         //     are looking at Function,
//         //
//         //     /// A regular atom that corresponds to a module
//         //     /// This helps to quickly distinguish what kind of internned string we
//         //     are looking at Module,
//         // };
//         //
//         // constexpr Handle(const StringTable& parent_in, const usize id_in,
//         //                  const usize len_in, Type type_in = Type::Empty) noexcept
//         //     : type(type_in), id(id_in), len(len_in), parent(parent_in) {}
//         // const Type type{Type::Empty};

//         const usize id;
//         const usize len;

//         constexpr Handle(const StringTable& parent_in, const usize id_in,
//                          const usize len_in) noexcept
//             : id(id_in), len(len_in), parent(parent_in) {}

//         static constexpr Handle none(const StringTable& parent) noexcept {
//             return Handle(parent, 0, 0);
//         }

//         /// Same as Handle::none(const StrinTable&), but uses
//         /// the global static StringTable::instance as its parent
//         static constexpr Handle make_none() noexcept {
//             return Handle::none(StringTable::instance());
//         }

//         /// Creates a new handle with id and len.
//         /// Uses the global static StringTable Instance for its parent
//         static constexpr Handle make(const usize id, const usize len) noexcept {
//             return Handle(StringTable::instance(), id, len);
//         }

//         constexpr Str get() const noexcept {
//             if (const auto opt = this->parent.get(*this)) {
//                 return opt.value();
//             }
//             return {};
//         }

//         constexpr bool is_none() const noexcept {
//             return this->id == 0 && this->len == 0;
//         }

//         constexpr operator bool() const noexcept { return !this->is_none(); }

//         constexpr bool operator==(const Handle& rhs) const noexcept {
//             if (&rhs != this) {
//                 return this->id == rhs.id;
//             }
//             return true;
//         }

//         constexpr bool operator==(const Str other) const noexcept {
//             return this->get() == other;
//         }

//         constexpr bool operator!=(const Str other) const noexcept {
//             return !(*this == other);
//         }

//         constexpr bool operator!=(const Handle& rhs) const noexcept {
//             return !(*this == rhs);
//         }

//        private:
//         const StringTable& parent;
//     };

//     template <const usize CAP = RESERVE_BYTES>
//     static constexpr StringTable with_capacity() noexcept {
//         StringTable st{};

//         // ensure below reserve call wont throw.
//         static_assert(CAP <= st.symbols.max_size(), "Capacity too large!");

//         // but lets put these in a try/catch just in case... (i hate exceptions
//         // btw ugh... lol)
//         try {
//             st.symbols.reserve(CAP);
//         } catch (const std::exception& e) {
//             // log exception, and return default constructed SymbolTable.
//             PLOGE << "Error reserving capacity for SymbolTable!\n\t=> " << e.what();
//         }

//         return st;
//     }

//     /// Gets a static singleton instance of StringTable with
//     /// StringTable::STATIC_CAP * 24 bytes reserved
//     static constexpr StringTable& instance() noexcept {
//         return StringTable::instance_sized<RESERVE_BYTES>();
//     }

//     /// Gets a static singleton instance of StringTable with
//     /// StringTable::STATIC_CAP * 24 bytes reserved
//     /// @remarks Keep in mind this will create an entirely new static instance for
//     /// each call to this function with a differently sized CAP template parameter.
//     /// So it is possible to have 2 separate static instances, ex:
//     ///
//     ///
//     ///
//     /// @code{.cpp}
//     ///
//     ///
//     ///
//     /// StringTable& t1 = StringTable::instance_sized<>();
//     /// StringTable& t2 = StringTable::instance_sized<64>();
//     /// assert(&t1 != &t2);
//     ///
//     ///
//     ///
//     /// @endcode
//     template <const usize CAP>
//     static constexpr StringTable& instance_sized() noexcept {
//         static auto self = StringTable::with_capacity<CAP>();
//         return self;
//     }

//     /// Adds symbol to symbol table. Returns handle to added symbol, or handle to
//     /// existing symbol if given symbol is already in table
//     Handle add(const Str symbol) noexcept;
//     /// Linearly scans through buffer for @param symbol.
//     Opt<Str> lookup_str(const Str symbol) const noexcept;
//     /// @brief  Linearly scans through buffer for @param symol
//     /// @returns std::optional<Handle> if found, std::nullopt otherwise
//     Opt<Handle> lookup_handle(const Str symbol) const noexcept;
//     /// Indexes internal string table to get a view that points to requested symbol
//     Opt<Str> get(const Handle handle) const noexcept;

//     /// Same as lookup_str, but also looks for @param ':' + name as well
//     Opt<Str> lookup_or_rune_str(const Str name) const noexcept;

//     /// Same as lookup_handle, but also looks for @param ':' + name if not found with
//     /// @param name alone
//     Opt<Handle> lookup_or_rune_handle(const Str symbol) const noexcept;

//     constexpr bool has(const Str name) const noexcept {
//         return this->lookup_str(name).has_value();
//     }

//     StringTable(StringTable&&) noexcept = default;
//     StringTable& operator=(StringTable&&) noexcept = default;

//    private:
//     StringTable(const StringTable&) = delete;
//     StringTable& operator=(const StringTable&) = delete;

//     Handle append(const Str name);

//     Str substr(const usize id, const usize len) const noexcept;
//     constexpr Str substr(const Handle handle) const noexcept {
//         return this->substr(handle.id, handle.len);
//     }

//     String symbols{};
// };

// constexpr bool operator==(const Str lhs, const StringTable::Handle& rhs) noexcept {
//     return lhs == rhs.get();
// }

// constexpr bool operator!=(const Str lhs, const StringTable::Handle& rhs) noexcept {
//     return !(lhs == rhs);
// }
// }  // namespace zeal::core
