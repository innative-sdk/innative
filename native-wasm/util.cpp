// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#include "util.h"
#include <assert.h>
#include <stdexcept>
#include <intrin.h>

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
#include <cpuid.h>
#error TODO
#endif

uint64_t DecodeLEB128(Stream& s, ERROR_CODE& err, unsigned int maxbits, bool sign)
{
  size_t shift = 0;
  int byte = 0;
  uint64_t result = 0;
  do
  {
    byte = s.Get();
    if(byte == -1)
    {
      err = ERR_PARSE_UNEXPECTED_EOF;
      return 0;
    }

    result |= ((byte&0x7F) << shift);
    shift += 7;
  } while((byte & 0x80) != 0); // Don't break based on maxbits because padding bytes are allowed

  if(((~0ULL) << maxbits) & result)
    err = ERR_FATAL_INVALID_ENCODING;

  if(sign && (byte & 0x40))
    result |= (~0ULL << shift);

  err = ERR_SUCCESS;
  return result;
}

FunctionSig* ModuleFunction(Module& m, varuint32 index)
{
  if(index < m.importsection.functions)
    return &m.type.functions[m.importsection.imports[index].sig_index];
  index -= m.importsection.functions;
  if(index < m.function.n_funcdecl)
    return &m.type.functions[m.function.funcdecl[index]];
  return 0;
}
TableDesc* ModuleTable(Module& m, varuint32 index)
{
  size_t i = index + m.importsection.functions; // Shift index to table section
  if(i < m.importsection.tables)
    return &m.importsection.imports[i].table_desc;
  i -= m.importsection.tables;
  if(i < m.table.n_tables)
    return &m.table.tables[i];
  return 0;
}
MemoryDesc* ModuleMemory(Module& m, varuint32 index)
{
  size_t i = index + m.importsection.tables; // Shift index to memory section
  if(i < m.importsection.memory)
    return &m.importsection.imports[i].mem_desc;
  i -= m.importsection.memory;
  if(i < m.memory.n_memory)
    return &m.memory.memory[i];
  return 0;
}
GlobalDesc* ModuleGlobal(Module& m, varuint32 index)
{
  size_t i = index + m.importsection.memory; // Shift index to globals section
  if(i < m.importsection.globals)
    return &m.importsection.imports[i].global_desc;
  i -= m.importsection.globals;
  if(i < m.global.n_globals)
    return &m.global.globals[i].desc;
  return 0;
}
std::pair<Module*, Export*> ResolveExport(Environment& env, Import& imp)
{
  khint_t iter = kh_get_modules(env.modulemap, (char*)imp.module_name.bytes);
  if(iter == kh_end(env.modulemap))
    return { 0,0 };

  varuint32 i = kh_value(env.modulemap, iter);
  if(i >= env.n_modules)
    return { 0,0 };

  iter = kh_get_exports(env.modules[i].exports, (char*)imp.export_name.bytes);
  if(iter == kh_end(env.modules[i].exports))
    return { env.modules + i, 0 };

  varuint32 j = kh_value(env.modules[i].exports, iter);
  if(j >= env.modules[i].exportsection.n_exports)
    return { env.modules + i, 0 };

  return { env.modules + i, env.modules[i].exportsection.exports + j };
}

std::string GetProgramPath()
{
  std::string buf;
#ifdef NW_PLATFORM_WIN32
  buf.reserve(MAX_PATH);
  buf.resize(GetModuleFileNameA(NULL, const_cast<char*>(buf.data()), buf.capacity()));
#elif defined(NW_PLATFORM_POSIX)
#error TODO
#endif
  return buf;
}

void GetCPUInfo(uintcpuinfo& info, int flags)
{
#ifdef NW_PLATFORM_WIN32
  SYSTEM_INFO sysinfo;
  GetSystemInfo(&sysinfo);
  info[5] = sysinfo.wProcessorArchitecture | (flags << 16);
  __cpuid(info, 1);
#elif defined(NW_PLATFORM_POSIX)
#error TODO
#endif
}

#ifdef NW_PLATFORM_WIN32
void* LoadDLL(const char* path) { return LoadLibraryA(path); }
void* LoadDLLFunction(void* dll, const char* name) { return GetProcAddress((HMODULE)dll, name);}
void FreeDLL(void* dll) { FreeLibrary((HMODULE)dll); }
#elif defined(NW_PLATFORM_POSIX)
void* LoadDLL(const char* path) { return dlopen(path, RTLD_LAZY); }
void* LoadDLLFunction(void* dll, const char* name) { return dlsym(dll, name); }
void FreeDLL(void* dll) { dlclose(dll); }
#endif