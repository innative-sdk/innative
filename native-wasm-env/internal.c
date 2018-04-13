// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#include "native-wasm/export.h"
#include <string.h>

#ifdef NW_PLATFORM_WIN32
#pragma pack(push)
#pragma pack(8)
#define WINVER 0x0501 //_WIN32_WINNT_WINXP   
#define _WIN32_WINNT 0x0501
#define NTDDI_VERSION 0x05010300 //NTDDI_WINXPSP3 
#define WIN32_LEAN_AND_MEAN
#ifndef NOMINMAX // Some compilers enable this by default
#define NOMINMAX
#endif
#define NODRAWTEXT
#define NOBITMAP
#define NOMCX
#define NOSERVICE
#define NOHELP
#define NOGDI
#include <windows.h>
#pragma pack(pop)
#elif defined(NW_PLATFORM_POSIX)
#error TODO
#else
#error unknown platform!
#endif

// Platform-specific implementation of the mem.size instruction, except it works in bytes
NW_COMPILER_DLLEXPORT extern uint64_t _native_wasm_internal_env_memory_size(void* p)
{
#ifdef NW_PLATFORM_WIN32
  return HeapSize(GetProcessHeap(), 0, p);
#endif
}

// Platform-specific implementation of the mem.grow instruction, except it works in bytes
NW_COMPILER_DLLEXPORT extern void* _native_wasm_internal_env_grow_memory(void* p, uint64_t i)
{
#ifdef NW_PLATFORM_WIN32
  if(!p) // If null do initial allocation
    return HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, i);
  return HeapReAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, p, _native_wasm_internal_env_memory_size(p) + i);
#endif
}

// This simply uses this compiler's implementation of memcpy for our platform
NW_COMPILER_DLLEXPORT extern void* _native_wasm_internal_env_memcpy(void* dest, const void* src, uint64_t sz)
{
  return memcpy(dest, src, sz);
}