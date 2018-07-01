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

const char* NW_ENTRYPOINT = "__native_wasm_main_func";
const char* NW_GETCPUINFO = "__native_wasm_getcpuinfo";
const char* NW_EXTENSION = ".nw-cache";
const char* NW_ENV_EXTENSION = ".nw-env-cache";
const char* NW_GLUE_STRING = "_NWLNK_";

const char OPNAMES[][20] = {
  // Control flow operators
  "unreachable",           // 0x00
  "nop",                   // 0x01
  "block",                 // 0x02
  "loop",                  // 0x03
  "if",                    // 0x04
  "else",                  // 0x05
  "RESERVED",              // 0x06
  "RESERVED",              // 0x07
  "RESERVED",              // 0x08
  "RESERVED",              // 0x09
  "RESERVED",              // 0x0a
  "end",                   // 0x0b
  "br",                    // 0x0c
  "br_if",                 // 0x0d
  "br_table",              // 0x0e
  "return",                // 0x0f

  // Call operators
  "call",                  // 0x10
  "call_indirect",         // 0x11

  "RESERVED",              // 0x12
  "RESERVED",              // 0x13
  "RESERVED",              // 0x14
  "RESERVED",              // 0x15
  "RESERVED",              // 0x16
  "RESERVED",              // 0x17
  "RESERVED",              // 0x18
  "RESERVED",              // 0x19

  // Parametric operators
  "drop",                  // 0x1a
  "select",                // 0x1b

  "RESERVED",              // 0x1c
  "RESERVED",              // 0x1d
  "RESERVED",              // 0x1e
  "RESERVED",              // 0x1f

  // Variable access
  "get_local",             // 0x20
  "set_local",             // 0x21
  "tee_local",             // 0x22
  "get_global",            // 0x23
  "set_global",            // 0x24

  "RESERVED",              // 0x25
  "RESERVED",              // 0x26
  "RESERVED",              // 0x27

  // Memory-related operator
  "i32.load",              // 0x28
  "i64.load",              // 0x29
  "f32.load",              // 0x2a
  "f64.load",              // 0x2b
  "i32.load8_s",           // 0x2c
  "i32.load8_u",           // 0x2d
  "i32.load16_s",          // 0x2e
  "i32.load16_u",          // 0x2f
  "i64.load8_s",           // 0x30
  "i64.load8_u",           // 0x31
  "i64.load16_s",          // 0x32
  "i64.load16_u",          // 0x33
  "i64.load32_s",          // 0x34
  "i64.load32_u",          // 0x35
  "i32.store",             // 0x36
  "i64.store",             // 0x37
  "f32.store",             // 0x38
  "f64.store",             // 0x39
  "i32.store8",            // 0x3a
  "i32.store16",           // 0x3b
  "i64.store8",            // 0x3c
  "i64.store16",           // 0x3d
  "i64.store32",           // 0x3e
  "current_memory",        // 0x3f
  "grow_memory",           // 0x40

  // Constants
  "i32.const",             // 0x41
  "i64.const",             // 0x42
  "f32.const",             // 0x43
  "f64.const",             // 0x44

  // Comparison operators
  "i32.eqz",               // 0x45
  "i32.eq",                // 0x46
  "i32.ne",                // 0x47
  "i32.lt_s",              // 0x48
  "i32.lt_u",              // 0x49
  "i32.gt_s",              // 0x4a
  "i32.gt_u",              // 0x4b
  "i32.le_s",              // 0x4c
  "i32.le_u",              // 0x4d
  "i32.ge_s",              // 0x4e
  "i32.ge_u",              // 0x4f

  "i64.eqz",               // 0x50
  "i64.eq",                // 0x51
  "i64.ne",                // 0x52
  "i64.lt_s",              // 0x53
  "i64.lt_u",              // 0x54
  "i64.gt_s",              // 0x55
  "i64.gt_u",              // 0x56
  "i64.le_s",              // 0x57
  "i64.le_u",              // 0x58
  "i64.ge_s",              // 0x59
  "i64.ge_u",              // 0x5a

  "f32.eq",                // 0x5b
  "f32.ne",                // 0x5c
  "f32.lt",                // 0x5d
  "f32.gt",                // 0x5e
  "f32.le",                // 0x5f
  "f32.ge",                // 0x60

  "f64.eq",                // 0x61
  "f64.ne",                // 0x62
  "f64.lt",                // 0x63
  "f64.gt",                // 0x64
  "f64.le",                // 0x65
  "f64.ge",                // 0x66

  // Numeric operators
  "i32.clz",               // 0x67
  "i32.ctz",               // 0x68
  "i32.popcnt",            // 0x69
  "i32.add",               // 0x6a
  "i32.sub",               // 0x6b
  "i32.mul",               // 0x6c
  "i32.div_s",             // 0x6d
  "i32.div_u",             // 0x6e
  "i32.rem_s",             // 0x6f
  "i32.rem_u",             // 0x70
  "i32.and",               // 0x71
  "i32.or",                // 0x72
  "i32.xor",               // 0x73
  "i32.shl",               // 0x74
  "i32.shr_s",             // 0x75
  "i32.shr_u",             // 0x76
  "i32.rotl",              // 0x77
  "i32.rotr",              // 0x78

  "i64.clz",               // 0x79
  "i64.ctz",               // 0x7a
  "i64.popcnt",            // 0x7b
  "i64.add",               // 0x7c
  "i64.sub",               // 0x7d
  "i64.mul",               // 0x7e
  "i64.div_s",             // 0x7f
  "i64.div_u",             // 0x80
  "i64.rem_s",             // 0x81
  "i64.rem_u",             // 0x82
  "i64.and",               // 0x83
  "i64.or",                // 0x84
  "i64.xor",               // 0x85
  "i64.shl",               // 0x86
  "i64.shr_s",             // 0x87
  "i64.shr_u",             // 0x88
  "i64.rotl",              // 0x89
  "i64.rotr",              // 0x8a

  "f32.abs",               // 0x8b
  "f32.neg",               // 0x8c
  "f32.ceil",              // 0x8d
  "f32.floor",             // 0x8e
  "f32.trunc",             // 0x8f
  "f32.nearest",           // 0x90
  "f32.sqrt",              // 0x91
  "f32.add",               // 0x92
  "f32.sub",               // 0x93
  "f32.mul",               // 0x94
  "f32.div",               // 0x95
  "f32.min",               // 0x96
  "f32.max",               // 0x97
  "f32.copysign",          // 0x98

  "f64.abs",               // 0x99
  "f64.neg",               // 0x9a
  "f64.ceil",              // 0x9b
  "f64.floor",             // 0x9c
  "f64.trunc",             // 0x9d
  "f64.nearest",           // 0x9e
  "f64.sqrt",              // 0x9f
  "f64.add",               // 0xa0
  "f64.sub",               // 0xa1
  "f64.mul",               // 0xa2
  "f64.div",               // 0xa3
  "f64.min",               // 0xa4
  "f64.max",               // 0xa5
  "f64.copysign",          // 0xa6

  // Conversions
  "i32.wrap/i64",          // 0xa7
  "i32.trunc_s/f32",       // 0xa8
  "i32.trunc_u/f32",       // 0xa9
  "i32.trunc_s/f64",       // 0xaa
  "i32.trunc_u/f64",       // 0xab

  "i64.extend_s/i32",      // 0xac
  "i64.extend_u/i32",      // 0xad
  "i64.trunc_s/f32",       // 0xae
  "i64.trunc_u/f32",       // 0xaf
  "i64.trunc_s/f64",       // 0xb0
  "i64.trunc_u/f64",       // 0xb1

  "f32.convert_s/i32",     // 0xb2
  "f32.convert_u/i32",     // 0xb3
  "f32.convert_s/i64",     // 0xb4
  "f32.convert_u/i64",     // 0xb5
  "f32.demote/f64",        // 0xb6

  "f64.convert_s/i32",     // 0xb7
  "f64.convert_u/i32",     // 0xb8
  "f64.convert_s/i64",     // 0xb9
  "f64.convert_u/i64",     // 0xba
  "f64.promote/f32",       // 0xbb

  // Reinterpretations
  "i32.reinterpret/f32",   // 0xbc
  "i64.reinterpret/f64",   // 0xbd
  "f32.reinterpret/i32",   // 0xbe
  "f64.reinterpret/i64"    // 0xbf
};

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

    result |= ((byte & 0x7F) << shift);
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
    return &m.type.functions[m.importsection.imports[index].func_desc.sig_index];
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

void* GreedyAlloc(size_t n)
{
  return malloc(n);
}

NWPath GetProgramPath()
{
  std::string buf;
#ifdef NW_PLATFORM_WIN32
  buf.resize(MAX_PATH);
  buf.resize(GetModuleFileNameA(NULL, const_cast<char*>(buf.data()), buf.capacity()));
#elif defined(NW_PLATFORM_POSIX)
#error TODO
#endif
  return NWPath(std::move(buf));
}

NWPath GetWorkingDir()
{
  std::string buf;
#ifdef NW_PLATFORM_WIN32
  buf.resize(GetCurrentDirectoryA(0, 0));
  buf.resize(GetCurrentDirectoryA(buf.capacity(), const_cast<char*>(buf.data())));
#elif defined(NW_PLATFORM_POSIX)
#error TODO
#endif
  return NWPath(std::move(buf));
}

bool SetWorkingDir(const char* path)
{
#ifdef NW_PLATFORM_WIN32
  return SetCurrentDirectoryA(path) != 0;
#elif defined(NW_PLATFORM_POSIX)
#error TODO
#endif
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


std::string StrFormat(const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  std::string s;
  s.resize(vsnprintf(0, 0, fmt, args)+1);
  s.resize(vsnprintf((char*)s.data(), s.capacity(), fmt, args));
  va_end(args);
  return s;
}


#ifdef NW_PLATFORM_WIN32
void* LoadDLL(const char* path) { return LoadLibraryA(path); }
void* LoadDLLFunction(void* dll, const char* name) { return GetProcAddress((HMODULE)dll, name); }
void FreeDLL(void* dll) { FreeLibrary((HMODULE)dll); }
#elif defined(NW_PLATFORM_POSIX)
void* LoadDLL(const char* path) { return dlopen(path, RTLD_LAZY); }
void* LoadDLLFunction(void* dll, const char* name) { return dlsym(dll, name); }
void FreeDLL(void* dll) { dlclose(dll); }
#endif
