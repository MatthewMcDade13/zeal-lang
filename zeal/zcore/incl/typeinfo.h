#pragma once

#include "common.h"
#include "string_table.h"
namespace zeal::core {

/// simple POD value with minimal information about a type.
/// only contains fields for the actual id (type index in type table) and
/// the type name. May eventualy include more metadata, but not now
struct Typeid {
    u32 id;
    core::StringTable::Handle name;
};

// TODO: Later we will need to impl more than just primitive types
// and to support language user defined types. for now bool, ints, floats and strings
// will do will also need hashmap/object/table support as well as i want Zeal to have
// table syntax like js/lua/zig ect

struct TypeTable {};
}  // namespace zeal::core
