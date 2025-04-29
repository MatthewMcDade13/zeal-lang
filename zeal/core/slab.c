#include "slab.h"
#include <stdio.h>
#include <stdlib.h>
#include <threads.h>


typedef struct Slab {
  u8 *begin;
  u8 *end;


  usize block_size;
  usize cap_bytes;

  usize block_len;
} Slab;


static void _init_slab(Slab* slab, const usize block_size, const usize block_len);

Slab const* zvm_static_slab_init(const usize block_size, const usize block_len) {
  thread_local static int is_init = 0;
  thread_local static Slab slab;

  if (is_init == 0) {
    is_init = 1;
    _init_slab(&slab, block_size, block_len);
  }

  return &slab;
}

void zvm_static_slab_free(Slab *slab) {
 if (slab && slab->begin) {
   free(slab->begin);
   slab->begin = 0;
   slab->end = 0;
   slab->block_len = 0;
   slab->cap_bytes = 0;
   slab->block_len = 0;
 } 
 return;
}



void _init_slab(Slab* slab, const usize block_size, const usize block_len) {
  u8 *begin = calloc(block_len, block_size );
  if (begin != NULL) {
    slab->begin = begin;
    slab->end = begin + block_len * block_size;
    slab->block_size = block_size;
    slab->block_len = block_len;
    slab->cap_bytes = block_len * block_size;
  } else {
    puts("Cannot initialize new SlabAllocator. Out of memory!");
    exit(1);
  }
}
