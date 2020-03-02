// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__SCHEMA_H
#define IN__SCHEMA_H

#include "innative/innative.h"
#include "innative/khash.h"
#include "innative/errors.h"
#include "innative/opcodes.h"
#include "innative/flags.h"
#include "innative/module.h"
#include "innative/sourcemap.h"
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>

#define kh_exist2(h, x) ((x < kh_end(h)) && kh_exist(h, x))
#define MUTABLE

#ifdef __cplusplus
  #undef MUTABLE
  #define MUTABLE mutable
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

// WASM binary type encodings, stored as a varsint7
enum WASM_TYPE_ENCODING
{
  TE_i32     = -0x01,
  TE_i64     = -0x02,
  TE_f32     = -0x03,
  TE_f64     = -0x04,
  TE_funcref = -0x10,
  TE_cref    = -0x19,
  TE_func    = -0x20,
  TE_void    = -0x40,

  TE_iPTR = 0x70, // Internal values, never encoded
  TE_NONE = 0x71,
  TE_POLY = 0x72,
};

// Flags applicable to resizable limits
enum WASM_LIMIT_FLAGS
{
  WASM_LIMIT_HAS_MAXIMUM = 0x01,
};

// Known webassembly section opcodes
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

// Export or import kind enumeration.
enum WASM_KIND
{
  WASM_KIND_FUNCTION = 0,
  WASM_KIND_TABLE    = 1,
  WASM_KIND_MEMORY   = 2,
  WASM_KIND_GLOBAL   = 3,
};

struct IN_WASM_ENVIRONMENT;

// Represents a binary webassembly byte array structure
typedef struct IN_WASM_BYTE_ARRAY
{
#ifdef __cplusplus
  IN_WASM_BYTE_ARRAY() : n_bytes(0), bytes(nullptr) {}
  IN_WASM_BYTE_ARRAY(uint8_t* b, varuint32 n) : n_bytes(n), bytes(b) {}
  inline uint8_t* get() { return bytes; }
  inline const uint8_t* get() const { return bytes; }
  inline const char* str() const { return !bytes ? "" : reinterpret_cast<const char*>(bytes); }
  inline varuint32 size() const { return n_bytes; }
  void resize(varuint32 sz, bool terminator, const struct IN_WASM_ENVIRONMENT& env);
  void discard(varuint32 sz, bool terminator);

  bool operator==(const IN_WASM_BYTE_ARRAY& r) const;
  inline bool operator!=(const IN_WASM_BYTE_ARRAY& r) const { return !operator==(r); }
  inline const uint8_t& operator[](varuint32 i) const
  {
    assert(i < n_bytes);
    return bytes[i];
  }
  inline uint8_t& operator[](varuint32 i)
  {
    assert(i < n_bytes);
    return bytes[i];
  }

  static IN_WASM_BYTE_ARRAY Identifier(const char* s, size_t l)
  {
    return IN_WASM_BYTE_ARRAY(reinterpret_cast<uint8_t*>(const_cast<char*>(s)), static_cast<varuint32>(l));
  }

protected:
#endif
  varuint32 n_bytes;
  uint8_t* bytes;
} ByteArray;

typedef ByteArray Identifier;

KHASH_DECLARE(exports, Identifier, varuint32);
KHASH_DECLARE(cimport, Identifier, char);
KHASH_DECLARE(modules, Identifier, size_t);

// A custom debug info structure used to map webassembly debug information.
typedef struct IN_WASM_DEBUGINFO
{
  unsigned int line;
  unsigned int column;
  Identifier name; // Stored debug name, if applicable
} DebugInfo;

// A union representing all possible immediate values of a webassembly instruction.
typedef union IN_WASM_IMMEDIATE
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
  struct
  {
    varuint32 n_table;
    varuint32* table;
  };
} Immediate;

// Encodes a single webassembly instruction and it's associated immediate values, plus it's location in the source.
typedef struct IN_WASM_INSTRUCTION
{
  uint8_t opcode;
  Immediate immediates[MAX_IMMEDIATES];
  unsigned int line; // To keep the size small, we ONLY store line/column on instructions
  unsigned int column;
} Instruction;

// A webassembly function type signature, encoding the form, parameters, and return values.
typedef struct IN_WASM_FUNCTION_TYPE
{
  varsint7 form;
  varsint7* params;
  varuint32 n_params;
  varsint7* returns;
  varuint32 n_returns;
} FunctionType;

// The underlying resizable limits structure used by linear memories and tables.
typedef struct IN_WASM_RESIZABLE_LIMITS
{
  varuint32 flags; // WASM_LIMIT_FLAGS
  varuint32 minimum;
  varuint32 maximum;
} ResizableLimits;

// A single linear memory declaration.
typedef struct IN_WASM_MEMORY_DESC
{
  ResizableLimits limits;
  DebugInfo debug;
} MemoryDesc;

// A single table declaration.
typedef struct IN_WASM_TABLE_DESC
{
  varsint7 element_type;
  ResizableLimits resizable;
  DebugInfo debug;
} TableDesc;

// A single global description.
typedef struct IN_WASM_GLOBAL_DESC
{
  varsint7 type;
  varuint1 mutability;
  DebugInfo debug;
} GlobalDesc;

// A single global declaration, which is a description plus an initialization instruction.
typedef struct IN_WASM_GLOBAL_DECL
{
  GlobalDesc desc;
  Instruction init;
} GlobalDecl;

// A single function description, which encodes both a type index telling us the function type signature, and the debug
// information associated with it.
typedef struct IN_WASM_FUNCTION_DESC
{
  varuint32 type_index;
  DebugInfo debug;
  DebugInfo* param_debug; // Always the size of n_params from the signature, or NULL
} FunctionDesc;

// Represents a single webassembly import definition
typedef struct IN_WASM_IMPORT
{
  Identifier module_name;
  Identifier export_name;
  varuint7 kind;          // WASM_KIND
  MUTABLE bool alternate; // Internal flag - if true, this import's canonical name always includes the module name
  MUTABLE bool ignore;
  union
  {
    FunctionDesc func_desc;
    TableDesc table_desc;
    MemoryDesc mem_desc;
    GlobalDesc global_desc;
  };
} Import;

// Represents a single webassembly export definition
typedef struct IN_WASM_EXPORT
{
  Identifier name;
  varuint7 kind; // WASM_KIND
  varuint32 index;
} Export;

// Encodes initialization data for a table
typedef struct IN_WASM_TABLE_INIT
{
  varuint32 index;
  Instruction offset;
  varuint32 n_elements;
  varuint32* elements;
} TableInit;

// Stores a local declaration, which includes the count and debug information.
typedef struct IN_WASM_FUNCTION_LOCAL
{
  varuint32 count; // We need to keep track of this to reconstruct arrays
  varsint7 type;
  DebugInfo debug; // If a name section specifies a local in the middle of a compressed array, we split it off
} FunctionLocal;

// Defines the locals, instructions, and debug information for a webassembly function body
typedef struct IN_WASM_FUNCTION_BODY
{
  FunctionLocal* locals;
  varuint32 n_locals;
  varuint32 local_size; // total number of individual locals (sum of all counts)
  Instruction* body;
  varuint32 n_body;    // track actual number of instructions
  varuint32 body_size; // track number of bytes used by instruction section
  unsigned int line;
  unsigned int column;
} FunctionBody;

// Encodes initialization data for a data section
typedef struct IN_WASM_DATA_INIT
{
  varuint32 index;
  Instruction offset;
  ByteArray data;
} DataInit;

// Represents any custom section defined in the module
typedef struct IN_WASM_CUSTOM_SECTION
{
  varuint7 opcode;
  varuint32 payload;
  Identifier name; // The first thing in a custom section must always be a legal identifier
  const uint8_t* data;
} CustomSection;

#ifdef __cplusplus
namespace innative {
  struct Compiler;
}
typedef innative::Compiler IN_CODE_compiler;
namespace llvm {
  class LLVMContext;
}
typedef llvm::LLVMContext LLVM_LLVM_compiler;
#else
typedef void IN_CODE_compiler;
typedef void LLVM_LLVM_compiler;
#endif

// Represents a single webassembly module
typedef struct IN_WASM_MODULE
{
  uint32 magic_cookie;
  uint32 version;
  uint32_t knownsections; // bit-index corresponds to that OPCODE section being loaded
  Identifier name;        // Name of the module as determined by the environment or name section

  struct TypeSection
  {
    varuint32 n_functypes;
    FunctionType* functypes;
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
    FunctionDesc* funcdecl;
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
  int start_line; // The start function line is used to represent the entry point function to make debugging easier

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
  SourceMap* sourcemap;

  struct kh_exports_s* exports;
  const char* filepath;   // For debugging purposes, store path to the original file, if it exists
  IN_CODE_compiler* cache; // If non-zero, points to a cached compilation of this module
} Module;

// Represents a single validation error node in a singly-linked list.
typedef struct IN_WASM_VALIDATION_ERROR
{
  int code;
  char* error;
  ptrdiff_t m;
  struct IN_WASM_VALIDATION_ERROR* next;
} ValidationError;

// Encodes a single webassembly embedding used by the environment in the linking process.
typedef struct IN_WASM_EMBEDDING
{
  const void* data;
  uint64_t size; // If size is 0, data points to a null terminated UTF8 file path
  int tag; // defines the type of embedding data included, determined by the runtime. 0 is always a static library file for
           // the current platform.
  const char* name;
  struct IN_WASM_EMBEDDING* next;
} Embedding;

enum IN_LOG_LEVEL
{
  LOG_NONE  = -1, // Suppress all log output no matter what
  LOG_FATAL = 0,
  LOG_ERROR,
  LOG_WARNING, // Default setting
  LOG_NOTICE,  // Verbose setting
  LOG_DEBUG,   // Only useful for library developers
};

KHASH_DECLARE(modulepair, kh_cstr_t, FunctionType);

struct IN_WASM_ALLOCATOR;

// Represents a collection of webassembly modules and configuration options that will be compiled into a single binary
typedef struct IN_WASM_ENVIRONMENT
{
  size_t n_modules;        // number of completely loaded modules (for multithreading)
  size_t size;             // Size of loaded or loading modules
  size_t capacity;         // Capacity of the modules array
  Module* modules;         // Use AddModule() to manage this list
  Embedding* embeddings;   // Use AddEmbedding to manage this list
  ValidationError* errors; // A linked list of non-fatal validation errors that prevent proper execution.
  uint64_t flags;          // WASM_ENVIRONMENT_FLAGS
  uint64_t features;       // WASM_FEATURE_FLAGS
  uint64_t optimize;       // WASM_OPTIMIZE_FLAGS
  unsigned int maxthreads; // Max number of threads for any multithreaded action. If 0, there is no limit.
  const char* rootpath;    // Internal buffer for storing the root directory of the EXE to help with directory searches
  const char* libpath;     // Path to look for default environment libraries
  const char* objpath; // Path to store intermediate results. If NULL, intermediate results are stored in the output folder
  const char* linker;  // If nonzero, attempts to execute this path as a linker instead of using the built-in LLD linker
  const char* system;  // prefix for the "system" module, which simply attempts to link the function name as a C function.
                       // Defaults to a blank string.
  struct IN_WASM_ALLOCATOR* alloc; // Stores a pointer to the internal allocator
  int loglevel;                    // IN_LOG_LEVEL
  FILE* log;                       // Output stream for log messages
  void (*wasthook)(void*);         // Optional hook for WAST debugging cases
  const char** exports;            // Use AddCustomExport() to manage this list
  varuint32 n_exports;

  struct kh_modules_s* modulemap;
  struct kh_modulepair_s* whitelist;
  struct kh_cimport_s* cimports;
  LLVM_LLVM_compiler* context;
} Environment;

#ifdef __cplusplus
}
#endif

#endif
