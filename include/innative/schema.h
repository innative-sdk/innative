// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __SCHEMA_H__IN__
#define __SCHEMA_H__IN__

#include "innative/innative.h"
#include "innative/khash.h"
#include "innative/errors.h"
#include "innative/opcodes.h"
#include "innative/flags.h"
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>

#define kh_exist2(h, x) ((x < kh_end(h)) && kh_exist(h, x))

#ifdef  __cplusplus
#include <string>
extern "C" {
#endif

typedef uint32_t uint32;
typedef bool varuint1;
typedef uint8_t varuint7;
typedef uint32_t varuint32;
typedef uint64_t varuint64;
typedef int8_t varsint7;
typedef int32_t varsint32;
typedef int64_t varsint64;
typedef uint64_t varuptr;
typedef float float32;
typedef double float64;

// Maximum number of immediates used by any instruction
#define MAX_IMMEDIATES 2

enum WASM_TYPE_ENCODING
{
  TE_i32 = -0x01,
  TE_i64 = -0x02,
  TE_f32 = -0x03,
  TE_f64 = -0x04,
  TE_funcref = -0x10,
  TE_func = -0x20,
  TE_void = -0x40,

  TE_iPTR = 0x70, // Internal values, never encoded
  TE_NONE = 0x71,
  TE_POLY = 0x72,
};

enum WASM_LIMIT_FLAGS
{
  WASM_LIMIT_HAS_MAXIMUM = 0x01,
};

enum WASM_SECTION_OPCODE
{
  WASM_SECTION_CUSTOM   = 0x00,
  WASM_SECTION_TYPE     = 0x01,
  WASM_SECTION_IMPORT   = 0x02,
  WASM_SECTION_FUNCTION = 0x03,
  WASM_SECTION_TABLE    = 0x04,
  WASM_SECTION_MEMORY   = 0x05,
  WASM_SECTION_GLOBAL   = 0x06,
  WASM_SECTION_EXPORT   = 0x07,
  WASM_SECTION_START    = 0x08,
  WASM_SECTION_ELEMENT  = 0x09,
  WASM_SECTION_CODE     = 0x0A,
  WASM_SECTION_DATA     = 0x0B
};

enum WASM_KIND
{
  WASM_KIND_FUNCTION = 0,
  WASM_KIND_TABLE    = 1,
  WASM_KIND_MEMORY   = 2,
  WASM_KIND_GLOBAL   = 3
};

struct __WASM_ENVIRONMENT;

typedef struct __WASM_BYTE_ARRAY
{
#ifdef  __cplusplus
  __WASM_BYTE_ARRAY() : bytes(nullptr), n_bytes(0) {}
  __WASM_BYTE_ARRAY(uint8_t* b, varuint32 n) : bytes(b), n_bytes(n) {}
  inline uint8_t* get() { return bytes; }
  inline const uint8_t* get() const { return bytes; }
  inline const char* str() const { return (char*)bytes; }
  inline varuint32 size() const { return n_bytes; }
  void resize(varuint32 sz, bool terminator, const struct __WASM_ENVIRONMENT& env);
  void discard(varuint32 sz, bool terminator);

  bool operator==(const __WASM_BYTE_ARRAY& r) const;
  inline bool operator!=(const __WASM_BYTE_ARRAY& r) const { return !operator==(r); }
  inline const uint8_t& operator[](varuint32 i) const { assert(i < n_bytes); return bytes[i]; }
  inline uint8_t& operator[](varuint32 i) { assert(i < n_bytes); return bytes[i]; }

protected:
#endif
  varuint32 n_bytes;
  uint8_t* bytes;
} ByteArray;

typedef ByteArray Identifier;

KHASH_DECLARE(exports, Identifier, varuint32);
KHASH_DECLARE(cimport, Identifier, char);
KHASH_DECLARE(modules, Identifier, size_t);

typedef struct __WASM_DEBUGINFO
{
  unsigned int line;
  unsigned int column;
  Identifier name; // Stored debug name, if applicable
} DebugInfo;

typedef union __WASM_IMMEDIATE
{
  uint32 _uint32;
  varuint1 _varuint1;
  varuint7 _varuint7;
  varuint32 _varuint32;
  varuint64 _varuint64;
  varsint7 _varsint7;
  varsint32 _varsint32;
  varsint64 _varsint64;
  float32 _float32;
  float64 _float64;
  varuptr _varuptr;
  struct { varuint32 n_table; varuint32* table; };
} Immediate;

typedef struct __WASM_INSTRUCTION
{
  uint8_t opcode;
  Immediate immediates[MAX_IMMEDIATES];
  unsigned int line; // To keep the size small, we ONLY store line/column on instructions
  unsigned int column;
} Instruction;

typedef struct __WASM_FUNCTION_TYPE
{
  varsint7 form;
  varsint7* params;
  varuint32 n_params;
  varsint7* returns;
  varuint32 n_returns;
} FunctionType;

typedef struct __WASM_RESIZABLE_LIMITS
{
  varuint32 flags;
  varuint32 minimum;
  varuint32 maximum;
} ResizableLimits;

typedef struct __WASM_MEMORY_DESC
{
  ResizableLimits limits;
  DebugInfo debug;
} MemoryDesc;

typedef struct __WASM_TABLE_DESC
{
  varsint7 element_type;
  ResizableLimits resizable;
  DebugInfo debug;
} TableDesc;

typedef struct __WASM_GLOBAL_DESC
{
  varsint7 type;
  varuint1 mutability;
  DebugInfo debug;
} GlobalDesc;

typedef struct __WASM_GLOBAL_DECL
{
  GlobalDesc desc;
  Instruction init;
} GlobalDecl;

typedef struct __WASM_FUNCTION_DESC
{
  varuint32 type_index;
  DebugInfo debug;
  DebugInfo* param_names; // Always the size of n_params from the signature
} FunctionDesc;

typedef struct __WASM_IMPORT
{
  Identifier module_name;
  Identifier export_name;
  varuint7 kind; // WASM_KIND
  union
  {
    FunctionDesc func_desc;
    TableDesc table_desc;
    MemoryDesc mem_desc;
    GlobalDesc global_desc;
  };
} Import;

typedef struct __WASM_EXPORT
{
  Identifier name;
  varuint7 kind; // WASM_KIND
  varuint32 index;
} Export;

typedef struct __WASM_TABLE_INIT
{
  varuint32 index;
  Instruction offset;
  varuint32 n_elements;
  varuint32* elements;
} TableInit;

typedef struct __WASM_FUNCTION_BODY
{
  varuint32 body_size;
  varuint32 n_locals;
  varsint7* locals;
  Instruction* body;
  varuint32 n_body; // INTERNAL: track actual number of instructions
  DebugInfo* local_names; // INTERNAL: debug names of locals, always the size of n_locals or NULL if it doesn't exist
  DebugInfo* param_names; // INTERNAL: debug names of parameters, always the size of n_params or NULL if it doesn't exist
  DebugInfo debug;
} FunctionBody;

typedef struct __WASM_DATA_INIT
{
  varuint32 index;
  Instruction offset;
  ByteArray data;
} DataInit;

typedef struct __WASM_CUSTOM_SECTION
{
  varuint7 opcode;
  varuint32 payload;
  Identifier name; // The first thing in a custom section must always be a legal identifier
  uint8_t* data;
} CustomSection;

#ifdef  __cplusplus
namespace innative {
  namespace code {
    struct Context;
  }
}
typedef innative::code::Context __IN_CONTEXT;
namespace llvm {
  class LLVMContext;
}
typedef llvm::LLVMContext __LLVM_CONTEXT;
#else
typedef void __IN_CONTEXT;
typedef void __LLVM_CONTEXT;
#endif

typedef struct __WASM_MODULE
{
  uint32 magic_cookie;
  uint32 version;
  uint32_t knownsections; // bit-index corresponds to that OPCODE section being loaded
  Identifier name; // Name of the module as determined by the environment or name section

  struct TypeSection
  {
    varuint32 n_functions;
    FunctionType* functions;
  } type;

  struct ImportSection
  {
    varuint32 functions;
    varuint32 tables;
    varuint32 memories;
    union
    {
      varuint32 globals;
      varuint32 n_import;
    };
    Import* imports;
  } importsection;

  struct FunctionSection
  {
    varuint32 n_funcdecl;
    varuint32* funcdecl;
  } function;

  struct TableSection
  {
    varuint32 n_tables;
    TableDesc* tables;
  } table;

  struct MemorySection
  {
    varuint32 n_memories;
    MemoryDesc* memories;
  } memory;

  struct GlobalSection
  {
    varuint32 n_globals;
    GlobalDecl* globals;
  } global;

  struct ExportSection
  {
    varuint32 n_exports;
    Export* exports;
  } exportsection;

  varuint32 start;

  struct ElementSection
  {
    varuint32 n_elements;
    TableInit* elements;
  } element;

  struct CodeSection
  {
    varuint32 n_funcbody;
    FunctionBody* funcbody;
  } code;

  struct DataSection
  {
    varuint32 n_data;
    DataInit* data;
  } data;

  size_t n_custom;
  CustomSection* custom;

  struct kh_exports_s* exports;
  const char* path; // For debugging purposes, store path to source .wat file, if it exists.
  __IN_CONTEXT* cache; // If non-zero, points to a cached compilation of this module
} Module;

typedef struct __WASM_VALIDATION_ERROR
{
  int code;
  char* error;
  ptrdiff_t m;
  struct __WASM_VALIDATION_ERROR* next;
} ValidationError;

typedef struct __WASM_EMBEDDING
{
  const void* data;
  uint64_t size; // If size is 0, data points to a null terminated UTF8 file path
  int tag; // defines the type of embedding data included, determined by the runtime. 0 is always a static library file for the current platform.
  struct __WASM_EMBEDDING* next;
} Embedding;

enum WASM_LOG_LEVEL
{
  LOG_NONE = -1, // Suppress all log output no matter what
  LOG_FATAL = 0,
  LOG_ERROR,
  LOG_WARNING, // Default setting
  LOG_NOTICE, // Verbose setting
  LOG_DEBUG, // Only useful for library developers
};

KHASH_DECLARE(modulepair, kh_cstr_t, FunctionType);

struct __WASM_ALLOCATOR;

typedef struct __WASM_ENVIRONMENT
{
  size_t n_modules; // number of completely loaded modules (for multithreading)
  size_t size; // Size of loaded or loading modules
  size_t capacity; // Capacity of the modules array
  Module* modules;
  Embedding* embeddings;
  ValidationError* errors; //A linked list of non-fatal validation errors that prevent proper execution.
  uint64_t flags; // WASM_ENVIRONMENT_FLAGS
  uint64_t features; // WASM_FEATURE_FLAGS
  uint64_t optimize; // WASM_OPTIMIZE_FLAGS
  unsigned int maxthreads; // Max number of threads for any multithreaded action. If 0, there is no limit.
  const char* sdkpath; // Path to look for SDK components, which usually aren't in the working directory
  const char* linker; // If nonzero, attempts to execute this path as a linker instead of using the built-in LLD linker
  const char* system; // prefix for the "system" module, which simply attempts to link the function name as a C function. Defaults to a blank string.
  struct __WASM_ALLOCATOR* alloc; // Stores a pointer to the allocator
  int loglevel; // WASM_LOG_LEVEL
  FILE* log;
  void(*wasthook)(void*); // Optional hook for WAST debugging cases

  struct kh_modules_s* modulemap;
  struct kh_modulepair_s* whitelist;
  struct kh_cimport_s* cimports;
  __LLVM_CONTEXT* context;
} Environment;

#ifdef  __cplusplus
}
#endif

#endif