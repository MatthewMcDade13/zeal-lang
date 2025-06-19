#ifndef _ZEAL_ALLOC_INCL_STACK_H_
#define _ZEAL_ALLOC_INCL_STACK_H_

#include "common.h"


ZEAL_CAPI_BEGIN

typedef struct zl_ArenaAlloc zl_ArenaAlloc;
typedef zl_ArenaAlloc* const zl_Arena;

/// @breif Initialization options for Stack Allocator.
/// @warning Remember to 0 initialize!
typedef struct {
  /// Initial capacity if 0, then default is system OS Page size (usually around 4096, but not always)
  /// Will try to use stack before requesting memory through mmap or malloc
  int init_capacity;
  /// Track each elements size inline? default is false (to track size inline)
  bool no_track_size;  
  /// if 0, no limit to element size
  int max_elem_size;
} zl_ArenaDesc;

// extern const zl_StackAllocOpts DEFAULT_CONFIG;


// /// @breif Initializes the configuration for a global statck allocator.
// /// Use this if you want to specialize the global allocator before getting it with @see [zl_stackalloc_instance]. 
// /// @remarks if you dont call this before @see [zl_stackalloc_instance] then a default configuration is used
// void zl_stackalloc_static_init(const zl_StackAllocOpts opts);

// /// @breif Gets global static instance of Stack Allocator.
// /// Uses config from zl_stackalloc_static_init if it was called prior to a call
// /// to this function. otherwise a default config is used
// zl_Arena zl_stackalloc_instance(void);

// /// @breif Creates a non-static Stack Allocator with given opts
// zl_StackAllocator zl_stackalloc_create(const zl_StackAllocOpts opts);

// /// @warning ONLY USE FOR NON_STATIC Allocators!!!
// void zl_stackalloc_destroy(zl_StackAllocator* alloc);


// zl_StackSlot zl_stackalloc_push(zl_StackAllocator self, const void* data, const int size_bytes);
// zl_StackItem* zl_stackalloc_item_ref(zl_StackAllocator self, const zl_StackSlot slot);

// typedef bool (*zl_LookupPredicate)(const void* user_data, const int size_bytes);
// /// @breif Iterates backwared through stack, when predicate returns true,
// /// a StackItem reference is given back pointing to the user_data that predicate returned true for
// zl_StackItem* zl_stackalloc_lookup_item(zl_StackAllocator self, zl_LookupPredicate predicate);

// void zl_stackalloc_clear(zl_StackAllocator self);

ZEAL_CAPI_END

ZEAL_NAMESPACE(alloc)


ZEAL_NAMESPACE_END




#endif
