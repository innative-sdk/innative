// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include <stddef.h>
#include <stdint.h>

#define WASM_PAGE_POWER    16
#define WASM_PAGE_SIZE     (1 << WASM_PAGE_POWER)
#define MIN_ALLOC_POWER    4
#define MIN_ALLOC_SIZE     (1 << MIN_ALLOC_POWER)
#define ALLOC_GROWTH_POWER 2
#define ALLOC_GROWTH_RATE  (1 << ALLOC_GROWTH_POWER)

// A full binary tree of n levels has 2^n - 1 nodes, and the lowest level has half the nodes, so we count how many bytes
// are available, divide by 2, and multiply by the minimum allocation size to get how many bytes each bitmap page addresses.
// This is 2^p / 2 * 2^n, which is equal to 2^(p - 1 + n)
#define PAGE_ALLOC_POWER (WASM_PAGE_POWER + MIN_ALLOC_POWER - 1)
#define PAGE_ALLOC_SIZE  (1 << PAGE_ALLOC_POWER)

#define HEAP_PARENT(i) ((i - 1) / 2)
#define HEAP_LEFT(i)   ((i << 1) + 1)
#define HEAP_RIGHT(i)  ((i << 1) + 2)
#define HEAP_SIZE(i)   ((size_t)1 << (i + MIN_ALLOC_POWER - 1))

#ifdef TESTING_WASM
  #include <string.h>
  #define MALLOC  wasm_malloc
  #define FREE    wasm_free
  #define REALLOC wasm_realloc
  #define CALLOC  wasm_calloc
  #define EXPORT
size_t __builtin_wasm_memory_size(size_t memory);
size_t __builtin_wasm_memory_grow(size_t memory, size_t delta);
#else
  #define EXPORT  __attribute__((visibility("default")))
  #define MALLOC  malloc
  #define FREE    free
  #define REALLOC realloc
  #define CALLOC  calloc

EXPORT void* memcpy(void* pdest, const void* psrc, size_t sz)
{
  char* dest = (char*)pdest;
  char* src  = (char*)psrc;

  // Align dest pointer
  while((size_t)dest % sizeof(uint64_t) && sz)
  {
    *dest = *src;
    dest += 1;
    src += 1;
    sz -= 1;
  }

  // Very simple memcpy implementation because we don't have access to the C library
  while(sz > sizeof(uint64_t))
  {
    *((uint64_t*)dest) = *((uint64_t*)src);
    dest += sizeof(uint64_t);
    src += sizeof(uint64_t);
    sz -= sizeof(uint64_t);
  }
  while(sz)
  {
    *dest = *src;
    dest += 1;
    src += 1;
    sz -= 1;
  }

  return pdest;
}

EXPORT void* memset(void* ptr, int value, size_t num)
{
  uint64_t v = (value & 0xFF) * 0x0101010101010101ULL;
  uint8_t* p = (uint8_t*)ptr;

  // Align pointer
  for(; ((size_t)p % sizeof(uint64_t)) != 0 && num != 0; num -= 1)
  {
    *p = (uint8_t)value;
    p += 1;
  }

  for(; num > sizeof(uint64_t); num -= sizeof(uint64_t))
  {
    *((uint64_t*)p) = v;
    p += sizeof(uint64_t);
  }

  for(; num != 0; num -= 1)
  {
    *p = (uint8_t)value;
    p += 1;
  }

  return ptr;
}
#endif

EXPORT static uint8_t* HeapRoot = (uint8_t*)~0;

void _zero_memory(char* ptr, size_t num)
{
  // Align pointer
  for(; ((size_t)ptr % sizeof(uint64_t)) != 0 && num != 0; num -= 1)
  {
    *ptr = 0;
    ptr += 1;
  }

  for(; num > sizeof(uint64_t); num -= sizeof(uint64_t))
  {
    *((uint64_t*)ptr) = 0;
    ptr += sizeof(uint64_t);
  }

  for(; num != 0; num -= 1)
  {
    *ptr = 0;
    ptr += 1;
  }
}

void _wasm_allocate_to_end(uint8_t* target, uint8_t* end)
{
  if(target > end)
    __builtin_wasm_memory_grow(0, (target - end + WASM_PAGE_SIZE - 1) / WASM_PAGE_SIZE);
}

uint8_t* _find_bucket(void* ptr, size_t* capacity)
{
  uint8_t* end  = (uint8_t*)(__builtin_wasm_memory_size(0) * WASM_PAGE_SIZE);
  uint8_t* heap = HeapRoot;
  *capacity     = 1;

  while(heap < end)
  {
    uint8_t* mem  = heap + (*capacity * WASM_PAGE_SIZE);
    uint8_t* next = mem + (*capacity * PAGE_ALLOC_SIZE);
    if((uint8_t*)ptr >= mem && (uint8_t*)ptr < next)
      return heap;

    *capacity *= ALLOC_GROWTH_RATE;
    heap = next;
  }

  return 0;
}

size_t _find_indice(void* ptr, uint8_t* heap, size_t capacity, uint8_t* size)
{
  uint8_t* mem     = heap + (capacity * WASM_PAGE_SIZE);
  ptrdiff_t offset = (uint8_t*)ptr - mem;
  if(!heap || offset % MIN_ALLOC_SIZE)
    return (size_t)~0;

  uint8_t sz   = 1;
  size_t index = (offset / MIN_ALLOC_SIZE) + (capacity * WASM_PAGE_SIZE / 2) - 1;

  // Starting from the bottom of the heap (using the alignment of the pointer), walk up each level until we find an
  // allocated node
  while(heap[index] > 0)
  {
    if((index % 2) == 0) // All right-side children have an even index, where all left-side children are odd.
      return (size_t)~0; // An unallocated right-side pointer is an impossible because going up further in the tree violates
                         // alignment

    index = HEAP_PARENT(index);
    sz += 1;
  }

  *size = sz;
  return index;
}
/*
char _verify_ptr(void* ptr)
{
  size_t capacity;
  uint8_t* heap = _find_bucket(ptr, &capacity);
  uint8_t size;
  size_t index = _find_indice(ptr, heap, capacity, &size);
  int test     = heap[index];
  return index != (size_t)~0;
}

char _verify_heap(size_t index, uint8_t* heap, size_t capacity, char allocated)
{
  if(index >= (capacity * WASM_PAGE_SIZE) - 1)
    return 1;

  size_t l = HEAP_LEFT(index);
  size_t r = HEAP_RIGHT(index);
  if(!heap[index])
  {
    if(allocated) // If one of our parents was already allocated, this is illegal.
      return 0;
    allocated = 1;

    if(l < (capacity * WASM_PAGE_SIZE) - 1 && !heap[l] && !heap[r])
      allocated = 0;
  }
  else if(l < (capacity * WASM_PAGE_SIZE) - 1)
  {
    uint8_t ls = heap[l];
    uint8_t rs = heap[r];

    if(heap[index] != (ls > rs ? ls : rs))
    {
      if(ls != rs || heap[index] != ls + 1)
        return 0;
    }
  }

  return _verify_heap(r, heap, capacity, allocated) && _verify_heap(l, heap, capacity, allocated);
}

char _verify_heaps()
{
  uint8_t* end    = (uint8_t*)(__builtin_wasm_memory_size(0) * WASM_PAGE_SIZE);
  uint8_t* heap   = HeapRoot;
  size_t capacity = 1;

  while(heap < end)
  {
    if(!_verify_heap(0, heap, capacity, 0))
      return 0;
    heap += (capacity * WASM_PAGE_SIZE) + (capacity * PAGE_ALLOC_SIZE);
    capacity *= ALLOC_GROWTH_RATE;
  }

  return 1;
}*/

EXPORT void* MALLOC(size_t num)
{
  uint8_t* end = (uint8_t*)(__builtin_wasm_memory_size(0) * WASM_PAGE_SIZE);

  // The root of malloc is the only pointer in this entire program that can legally be 0, but the C++ compiler doesn't know
  // that, so we have to account for this
  if(HeapRoot == (uint8_t*)~0)
    HeapRoot = end;

  uint8_t* heap   = HeapRoot;
  size_t capacity = 1;
  uint8_t power   = 1;

  while(heap < end)
  {
    if(heap[0] > 0 && num <= HEAP_SIZE(heap[0]))
      break;

    // Use the size of this heap to find where the next heap should be
    heap += (capacity * WASM_PAGE_SIZE) + (capacity * PAGE_ALLOC_SIZE);
    capacity *= ALLOC_GROWTH_RATE;
    power += ALLOC_GROWTH_POWER;
  }

  size_t length = capacity * WASM_PAGE_SIZE;

  if(heap >= end) // If our current heap points past addressable memory, there are no buckets large enough for this allocation
  {
    _wasm_allocate_to_end(heap, end); // Make sure the most recent bucket (the one at root) has allocated all possible pages

    // Then we allocate pages for the bytemap, plus one additional page
    heap = (uint8_t*)(__builtin_wasm_memory_grow(0, capacity) * WASM_PAGE_SIZE);

    // Set up the free nodes
    size_t i     = 0;
    uint8_t size = WASM_PAGE_POWER - 1 + power;

    for(size_t j = 0; i < length - 1; ++j) // length - 1 is necessary because a heap takes up 2^n - 1
    {
      // for(size_t k = 0; k < ((size_t)1 << j); ++k)
      //  heap[i++] = size;
      memset(heap + i, size, (size_t)1 << j);
      i += (size_t)1 << j;
      size -= 1;
    }
    heap[i] = 0xFF; // Canary byte

    return MALLOC(num); // Then we recurse, because it's possible this new bucket still isn't big enough for this allocation
  }

  size_t index    = 0;
  size_t offset   = capacity * PAGE_ALLOC_SIZE;
  uint8_t* target = heap + (capacity * WASM_PAGE_SIZE);

  while(index < HEAP_PARENT(length - 1))
  {
    size_t l = HEAP_LEFT(index);
    size_t r = HEAP_RIGHT(index);
    size_t i = l;

    if(!heap[l] || HEAP_SIZE(heap[l]) < num)
      i = r;

    if(HEAP_SIZE(heap[i]) < num)
      break;
    index = i;

    // Apply offset only if this is a right-side cell
    offset >>= 1;
    if((index % 2) == 0)
      target += offset;
  }

  if(num > HEAP_SIZE(heap[index]))
    return 0;

  heap[index] = 0;

  while(index > 0)
  {
    index       = HEAP_PARENT(index);
    uint8_t l   = heap[HEAP_LEFT(index)];
    uint8_t r   = heap[HEAP_RIGHT(index)];
    heap[index] = r > l ? r : l; // We don't bother to check for the special free case here because its no longer possible.
  }

  _wasm_allocate_to_end(target + num, end);
  return target;
}

EXPORT void FREE(void* ptr)
{
  size_t capacity;
  uint8_t* heap = _find_bucket(ptr, &capacity);
  uint8_t size;
  size_t index = _find_indice(ptr, heap, capacity, &size);

  if(index == (size_t)~0)
  {
#ifdef _MSC_VER
  #ifdef _M_ARM64
    __break(-1);
  #elif defined(_M_ARM)
    __trap(-1);
  #else
    __ud2();
  #endif
#else
    __builtin_trap();
#endif
  }

  // Set this node as free, and recalculate it's parent free size
  heap[index] = size;

  while(index > 0)
  {
    index     = HEAP_PARENT(index);
    uint8_t l = heap[HEAP_LEFT(index)];
    uint8_t r = heap[HEAP_RIGHT(index)];
    if(l == size && r == size) // If both children are completely free, our true free size is both of them combined
      heap[index] = size + 1;  // size + 1 is actually the size times two since this is a power of two
    else
      heap[index] = l > r ? l : r;

    size += 1;
  }
}

EXPORT void* REALLOC(void* src, size_t num)
{
  if(!src)
    return MALLOC(num);

  size_t capacity;
  uint8_t* heap = _find_bucket(src, &capacity);
  uint8_t size;
  size_t n = _find_indice(src, heap, capacity, &size);

  if(HEAP_SIZE(size) >= num) // It's possible the existing allocation size is already enough to satisfy the resize
    return src;

  uint8_t old = size; // Store original size

  // If this is a left-side child and it's right-side sibling is 100% free, we can expand this allocation
  /*while(((n % 2) != 0) && heap[n + 1] == size) // This can never go past the root, because root is even
  {
    heap[n] = size; // Free our current node to preserve heap structure
    n = HEAP_PARENT(n); // Set n to our parent node
    heap[n] = 0; // Allocate our parent node. We don't need to move ptr even if we abandon our resize attempt.

    size_t index = n;
    while(index > 0)
    {
      index = HEAP_PARENT(index);
      uint8_t l = heap[HEAP_LEFT(index)];
      uint8_t r = heap[HEAP_RIGHT(index)];
      heap[index] = r > l ? r : l; // We don't bother to check for the special free case here because its no longer possible.
    }

    size += 1; // Fix size to be the size of our parent node;
    if(HEAP_SIZE(size) >= num) // If this satifies the resize attempt, return src
    {
      _wasm_allocate_to_end(src + num, (uint8_t*)(__builtin_wasm_memory_size(0) * WASM_PAGE_SIZE));
      return src;
    }
  }*/

  // We can't expand the allocation, so we just copy to a new one
  void* dest = MALLOC(num);
  memcpy(dest, src, old); // Copy over the old data
  FREE(src);              // Free the old memory
  return dest;
}

EXPORT void* CALLOC(size_t num, size_t size)
{
  size_t n = num * size;
  void* p  = MALLOC(n);
  _zero_memory((char*)p, n); // We DO need to zero this memory, because we didn't get it directly from memory.grow
  return p;
}