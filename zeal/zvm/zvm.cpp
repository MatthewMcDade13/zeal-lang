#include "zvm.h"
#include <iostream>

namespace zeal::zvm {



struct Zvm {
  
};


Zvm* zvm_create_instance(Zvm_InitConfig config) {
  
}

// Zvm_ErrorState zvm_stack_push_int(Zvm* z, int data);
Zvm_ErrorState zvm_stack_push(Zvm* z, void* data, zu32 size_bytes)  {
  
}



void* zvm_stack_peek_top(Zvm* z) {
  
}

Zvm_ErrorState zvm_load_bytecode(Zvm* z, const char* filepath) {
  
}

Zvm_ErrorState zvm_load_bytecode_memory(Zvm* z, const void* bytecode, zu32 size_bytes) {
  
}

Zvm_ErrorState zvm_compile_file(Zvm* z, const char* filepath) {
  
}

Zvm_ErrorState zvm_compile_memory(Zvm* z,const char* source) {
  
}

Zvm_ErrorState zvm_execute_memory(Zvm* z, const char* source) {
  
}

Zvm_ErrorState zvm_execute_file(Zvm* z, const char* filepath) {
  
}

Zvm_ErrorState zvm_execute_repl(Zvm* z) {
  
}

bool zvm_has_errors(Zvm* z) {
  
}

Zvm_ErrorInfo zvm_pop_error(Zvm* z) {
  
}

void zvm_delete_instance(Zvm* z) {
  
}


  
}

