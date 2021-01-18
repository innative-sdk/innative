// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "atomics.h"
#include "internal.h"
#include "wait_list.h"

#ifdef IN_PLATFORM_WIN32
  #include "../innative/win32.h"
#elif defined(IN_PLATFORM_POSIX)
// TODO: POSIX
#else
  #error unknown platform!
#endif

IN_COMPILER_DLLEXPORT int32_t _innative_internal_env_atomic_wait32(void* address, int32_t expected, int64_t timeoutns)
{
  struct in_wait_list* wait_list = _innative_internal_env_wait_map_get(&_innative_internal_env_global_wait_map, address, 1);
  _innative_internal_env_wait_list_enter(wait_list);

  if(_innative_internal_env_atomic_load32(address) != expected)
  {
    _innative_internal_env_wait_list_exit(wait_list);
    _innative_internal_env_wait_map_return(&_innative_internal_env_global_wait_map, address, wait_list);
    return 1; // "not-equal"
  }

  struct in_wait_entry* entry = _innative_internal_env_wait_list_push(wait_list);
  int32_t result              = _innative_internal_env_wait_entry_wait(wait_list, entry, timeoutns);

  _innative_internal_env_wait_list_remove(wait_list, entry);
  _innative_internal_env_wait_list_exit(wait_list);
  _innative_internal_env_wait_map_return(&_innative_internal_env_global_wait_map, address, wait_list);

  return result;
}

IN_COMPILER_DLLEXPORT int32_t _innative_internal_env_atomic_wait64(void* address, int64_t expected, int64_t timeoutns)
{
  struct in_wait_list* wait_list = _innative_internal_env_wait_map_get(&_innative_internal_env_global_wait_map, address, 1);
  _innative_internal_env_wait_list_enter(wait_list);

  if(_innative_internal_env_atomic_load64(address) != expected)
  {
    _innative_internal_env_wait_map_return(&_innative_internal_env_global_wait_map, address, wait_list);
    _innative_internal_env_wait_list_exit(wait_list);
    return 1; // "not-equal"
  }

  struct in_wait_entry* entry = _innative_internal_env_wait_list_push(wait_list);
  int32_t result              = _innative_internal_env_wait_entry_wait(wait_list, entry, timeoutns);

  _innative_internal_env_wait_list_remove(wait_list, entry);
  _innative_internal_env_wait_map_return(&_innative_internal_env_global_wait_map, address, wait_list);
  _innative_internal_env_wait_list_exit(wait_list);

  return result;
}

IN_COMPILER_DLLEXPORT uint32_t _innative_internal_env_atomic_notify(void* address, uint32_t count)
{
  struct in_wait_list* wait_list = _innative_internal_env_wait_map_get(&_innative_internal_env_global_wait_map, address, 0);
  if(wait_list == 0)
    return 0; // Nobody to notify

  _innative_internal_env_wait_list_enter(wait_list);

  uint32_t result = _innative_internal_env_wait_list_notify(wait_list, count);

  _innative_internal_env_wait_list_exit(wait_list);
  _innative_internal_env_wait_map_return(&_innative_internal_env_global_wait_map, address, wait_list);

  return result;
}

// Atomic helpers
#ifdef IN_PLATFORM_WIN32

// This code is based on the implementation of std::atomic in MSVC's headers
int32_t _innative_internal_env_atomic_load32(int32_t* address)
{
  volatile int32_t* vmem = (volatile int32_t*)address;
  int32_t value          = *vmem;
  _ReadWriteBarrier();
  return value;
}

int64_t _innative_internal_env_atomic_load64(int64_t* address)
{
  volatile int64_t* vmem = (volatile int64_t*)address;
  int64_t value;

  #if defined(_M_ARM)
  value = __ldrexd(vmem);
  #elif defined(_M_ARM64) || defined(_M_IX86)
  value = __iso_volatile_load64(vmem);
  #else // _M_X64
  value = *vmem;
  #endif

  #if defined(_M_ARM) || defined(_M_ARM64)
  __dmb(0xB);
  #else // _M_IX68 || _MX64
  _ReadWriteBarrier();
  #endif

  return value;
}

#elif defined(IN_PLATFORM_POSIX)

int32_t _innative_internal_env_atomic_load32(int32_t* address) { return __atomic_load_n(address, __ATOMIC_SEQ_CST); }
int64_t _innative_internal_env_atomic_load64(int64_t* address) { return __atomic_load_n(address, __ATOMIC_SEQ_CST); }

#endif
