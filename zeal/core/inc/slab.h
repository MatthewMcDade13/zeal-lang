#pragma once
#include "../typedefs.h"

#define MAX_LAYERS 255

#ifndef LAYER_COUNT
#define LAYER_COUNT 1
#endif
#if LAYER_COUNT >= MAX_LAYERS
#undef LAYER_COUNT
#define LAYER_COUNT MAX_LAYERS - 1
#endif

typedef struct BlockPool BlockPool;

typedef struct Slab Slab;

/// Creates a thread_local static slab allocator
Slab const* zvm_static_slab_init(const usize block_size, const usize block_len);

/// Frees memory allocated by a thread_local static slab allocator
void zvm_static_slab_free( Slab *slab);

/// Creates a new Slab allocator on the heap, this is so you can have
/// more than 1 slab allocator per thread.
void zvm_create_slab(const usize block_size, const usize block_len);

