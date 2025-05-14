#pragma once

#if defined(__cplusplus)

namespace zeal::zvm {
extern "C" {
#endif

typedef unsigned char zu8;
typedef char zi8;
typedef unsigned short zu16;
typedef short zi16;
typedef unsigned int zu32;
typedef int zi32;
typedef unsigned long int zu64;
typedef long int zi64;

/// A  non-owning view into some UTF-8 string`
typedef struct SliceStr {
  const char* str;
  zu32 length;
} SliceStr;

/// A non-owning view into some block of memory with
/// constant stride
typedef struct AnySlice {
  /// Pointer to begining of where slice points to
  void* data;
  /// Number of items this slice points to
  zu32 length;
  /// Size of each item in slice in bytes
  zu32 stride_bytes;
} AnySlice;

typedef struct Zvm Zvm;

typedef struct Zvm_InitConfig {
  /// if 0, will be set to 4kb default 
  zu16 initial_memory;
  /// File path to load as the root of execution
  const char* root_file_path;
} Zvm_InitConfig;

typedef enum Zvm_ErrorState {
  /// No error state, we good!
  ZVM_OK,
  ZVM_PARSE_ERROR,
  ZVM_IO_FILE_ERROR,
} Zvm_ErrorState;

typedef struct Zvm_ErrorInfo {
  Zvm_ErrorState error_id;
  const char* message;
} Zvm_ErrorInfo;

Zvm* zvm_create_instance(Zvm_InitConfig config);

// Zvm_ErrorState zvm_stack_push_int(Zvm* z, int data);
Zvm_ErrorState zvm_stack_push(Zvm* z, void* data, zu32 size_bytes);
void* zvm_stack_peek_top(Zvm* z);

Zvm_ErrorState zvm_load_bytecode(Zvm* z, const char* filepath);
Zvm_ErrorState zvm_load_bytecode_memory(Zvm* z, const void* bytecode, zu32 size_bytes);

Zvm_ErrorState zvm_compile_file(Zvm* z, const char* filepath);
Zvm_ErrorState zvm_compile_memory(Zvm* z,const char* source);

Zvm_ErrorState zvm_execute_memory(Zvm* z, const char* source);
Zvm_ErrorState zvm_execute_file(Zvm* z, const char* filepath);

Zvm_ErrorState zvm_execute_repl(Zvm* z);

bool zvm_has_errors(Zvm* z);
Zvm_ErrorInfo zvm_pop_error(Zvm* z);

void zvm_delete_instance(Zvm* z);





#if defined(__cplusplus)
}
}
#endif
