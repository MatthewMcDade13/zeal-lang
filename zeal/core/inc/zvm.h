#pragma once

typedef struct ZvmAllocator ZvmAllocator;


/// Gets the thread_local Zeal runtime allocator.
/// Be sure to initialize the allocator first before calling this function, if
/// it has not yet been called on this thread.
ZvmAllocator *get_allocator(void);

/// Initializes a Zeal Runtime Allocator
/// This is a global allocator that is static thread_local
void init_allocator(void);

void free_allocator(ZvmAllocator *alloc);



typedef struct Zvm Zvm;


void zvm_init(void);

void zvm_delete(void);

Zvm *zvm_instance(void);

