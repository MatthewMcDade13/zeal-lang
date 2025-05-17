#pragma once

// TODO: Im going to implement lang with LLVM or WebAssembly first
// so that i get a better idea of a good structure for bytecode before implementing my own


#include "common.h"
#include "conv.h"
namespace zeal::bc {


  enum class TypeId {
    Byte,
    Char,
    Int32,
    Int64,
    Float32,
    Float64,
    String,
    Bytes,
    Rune,
    Symbol,
    Function,
    /// Struct
    Struct,
    Module,
    /// Handle/Index to some other value,
    /// whos type will be decieded at runtime and/or after
    /// lookup
    Handle,
  };

  

 constexpr const u8 MAGIC_C[4] = {'Z', 'E', 'A', 'L'};
 constexpr const u32 MAGIC_INT = core::conv::le_bytes_to_uint32({MAGIC_C[0], MAGIC_C[1], MAGIC_C[2], MAGIC_C[3]});

  struct BytecodeHeader {
    const u32 magic = MAGIC_INT;
    
  };

  
}
