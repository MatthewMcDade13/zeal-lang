#include "string_table.h"

namespace zeal::core {

StringTable::Handle StringTable::add(const Str symbol) noexcept {
    if (const auto opt = this->lookup_handle(symbol)) {
        return opt.value();
    }
    try {
        return this->append(symbol);
    } catch (const std::exception& e) {
        PLOGE << "Unknown Error occured while appending symbol: "
              << (std::string(symbol) + "\0") << "\n\t=> " << e.what();
        return StringTable::Handle::none(*this);
    }
}

Opt<Str> StringTable::lookup_str(const Str symbol) const noexcept {
    if (const auto opt = this->lookup_handle(symbol)) {
        return this->substr(opt.value());
    }
    return std::nullopt;
}

Opt<StringTable::Handle> StringTable::lookup_handle(const Str name) const noexcept {
    if (const auto index = this->symbols.find(name); index != this->symbols.npos) {
        return StringTable::Handle(*this, index, name.size());
    }
    return std::nullopt;
}

Opt<Str> StringTable::get(const Handle handle) const noexcept {
    const auto view = this->substr(handle);
    if (view.size() == 0) {
        return std::nullopt;
    }
    return view;
}

/// Same as lookup, but also looks for @param ':' + name as well
Opt<Str> StringTable::lookup_or_rune_str(const Str name) const noexcept {
    if (const auto opt = this->lookup_str(name)) {
        return opt.value();
    } else if (const auto opt = this->lookup_str(":" + std::string(name))) {
        return opt.value();
    } else {
        return std::nullopt;
    }
}

StringTable::Handle StringTable::append(const Str name) {
    if (name.size() == 0) {
        PLOGE << "Cannot append string of length 0!";
        return StringTable::Handle::none(*this);
    }

    using std::operator""s;
    constexpr const char SPACER[] = "\0";
    const auto start = this->symbols.size();
    /// NOTE: This probably is not hte most efficeint, but insertations will mostly
    /// be limited to parsing, after that lookup will be more common, if at all. (as
    /// string_views can e used instead)
    const auto entry = std::string(name) + SPACER;
    this->symbols += entry;

    PLOGD << "Added Symbol: " << entry;
    return StringTable::Handle(*this, start, name.size());
}

Str StringTable::substr(const usize id, const usize len) const noexcept {
    const auto buflen = this->symbols.size();
    if (id >= buflen || (id + len) >= buflen) {
        PLOGW << "Attempted to get out of range substring! begin: "
              << std::to_string(id) << " end: " << std::to_string(id + len)
              << " length: " << std::to_string(len)
              << "indexed string is of length: "
              << std::to_string(this->symbols.size());
        return {};
    } else {
        const auto begin = this->symbols.begin() + id;
        const auto end = begin + len;
        if (end <= this->symbols.end()) {
            return Str(begin, end);
        } else {
            PLOGW << "Could not get substring that extends beyond string table "
                     "buffer size!";
            return {};
        }
    }
}

}  // namespace zeal::core
