#include "zvm.h"
#include "plog/Log.h"
#include "value.h"
#include <cassert>
#include <cstdlib>
#include <iostream>

namespace zeal::zvm {




// struct Zvm {

// private:
//   std::array<core::Value, ZVM_RUNTIME_STACK_SIZE> stack;
//   usize stack_top;
// };


Zvm* zvm_create_instance(Zvm_InitConfig config) {
  PLOGD << "TODO";
  assert(false);
  return nullptr;  
}

// Zvm_ErrorState zvm_stack_push_int(Zvm* z, int data);
Zvm_ErrorState zvm_stack_push(Zvm* z, void* data, zu32 size_bytes)  { 

  PLOGD << "TODO";
  assert(false);
  return Zvm_ErrorState::ZVM_IO_FILE_ERROR;
}



void* zvm_stack_peek_top(Zvm* z) {

  PLOGD << "TODO";
  assert(false);
  return nullptr;
}

Zvm_ErrorState zvm_load_bytecode(Zvm* z, const char* filepath) {

  PLOGD << "TODO";
  assert(false);
  return Zvm_ErrorState::ZVM_TODONIY;  
}

Zvm_ErrorState zvm_load_bytecode_memory(Zvm* z, const void* bytecode, zu32 size_bytes) {

  PLOGD << "TODO";
  assert(false);
  return Zvm_ErrorState::ZVM_TODONIY;  
}

Zvm_ErrorState zvm_compile_file(Zvm* z, const char* filepath) {

  PLOGD << "TODO";
  assert(false);

  return Zvm_ErrorState::ZVM_TODONIY;
}

Zvm_ErrorState zvm_compile_memory(Zvm* z,const char* source) {

  PLOGD << "TODO";
  assert(false);
  return Zvm_ErrorState::ZVM_TODONIY;  
}

Zvm_ErrorState zvm_execute_memory(Zvm* z, const char* source) {

  PLOGD << "TODO";
  assert(false);

  return Zvm_ErrorState::ZVM_TODONIY;
}

Zvm_ErrorState zvm_execute_file(Zvm* z, const char* filepath) {

  PLOGD << "TODO";
  assert(false);

  return Zvm_ErrorState::ZVM_TODONIY;  
}

Zvm_ErrorState zvm_execute_repl(Zvm* z) {

  PLOGD << "TODO";
  assert(false);
  return Zvm_ErrorState::ZVM_TODONIY;  


}

bool zvm_has_errors(Zvm* z) {

  PLOGD << "TODO";
  assert(false);
  return false;  
}

Zvm_ErrorInfo zvm_pop_error(Zvm* z) {

  PLOGD << "TODO";
  assert(false);
  return {};  
}

void zvm_delete_instance(Zvm* z) {

  PLOGD << "TODO";
  assert(false);
}


  
}

