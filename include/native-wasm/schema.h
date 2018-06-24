// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#ifndef __SCHEMA_H__NW__
#define __SCHEMA_H__NW__

#include "native-wasm/native-wasm.h"
#include "native-wasm/khash.h"
#include <stdint.h>
#include <stdbool.h>

#ifdef  __cplusplus
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
typedef varuint32 memflags;
typedef uint8_t byte;

const uint32 MAGIC_COOKIE = 0x6d736100;
const uint32 MAGIC_VERSION = 0x01;

// Maximum number of immediates used by any instruction
#define MAX_IMMEDIATES 2

KHASH_DECLARE(exports, kh_cstr_t, varuint32);
KHASH_DECLARE(modules, kh_cstr_t, varuint32);

enum TYPE_ENCODING
{
  TE_i32 = -0x01,
  TE_i64 = -0x02,
  TE_f32 = -0x03,
  TE_f64 = -0x04,
  TE_anyfunc = -0x10, 
  TE_func = -0x20,
  TE_void = -0x40,

  TE_iPTR = 0x70, // Internal values, never encoded
};

enum SECTION_OPCODE
{
  SECTION_CUSTOM   = 0x00,
  SECTION_TYPE     = 0x01,
  SECTION_IMPORT   = 0x02,
  SECTION_FUNCTION = 0x03,
  SECTION_TABLE    = 0x04,
  SECTION_MEMORY   = 0x05,
  SECTION_GLOBAL   = 0x06,
  SECTION_EXPORT   = 0x07,
  SECTION_START    = 0x08,
  SECTION_ELEMENT  = 0x09,
  SECTION_CODE     = 0x0A,
  SECTION_DATA     = 0x0B
};

enum WA_KIND
{
  KIND_FUNCTION = 0,
  KIND_TABLE    = 1,
  KIND_MEMORY   = 2,
  KIND_GLOBAL   = 3
};

enum INSTRUCTION_OPCODES
{
  OP_unreachable = 0x00,
  OP_nop = 0x01,
  OP_block = 0x02,
  OP_loop = 0x03,
  OP_if = 0x04,
  OP_else = 0x05,
  OP_end = 0x0b,
  OP_br = 0x0c,
  OP_br_if = 0x0d,
  OP_br_table = 0x0e,
  OP_return = 0x0f,

  // Call operators
  OP_call = 0x10,
  OP_call_indirect = 0x11,

  // Parametric operators
  OP_drop = 0x1a,
  OP_select = 0x1b,

  // Variable access
  OP_get_local = 0x20,
  OP_set_local = 0x21,
  OP_tee_local = 0x22,
  OP_get_global = 0x23,
  OP_set_global = 0x24,

  // Memory-related operator
  OP_i32_load = 0x28,
  OP_i64_load = 0x29,
  OP_f32_load = 0x2a,
  OP_f64_load = 0x2b,
  OP_i32_load8_s = 0x2c,
  OP_i32_load8_u = 0x2d,
  OP_i32_load16_s = 0x2e,
  OP_i32_load16_u = 0x2f,
  OP_i64_load8_s = 0x30,
  OP_i64_load8_u = 0x31,
  OP_i64_load16_s = 0x32,
  OP_i64_load16_u = 0x33,
  OP_i64_load32_s = 0x34,
  OP_i64_load32_u = 0x35,
  OP_i32_store = 0x36,
  OP_i64_store = 0x37,
  OP_f32_store = 0x38,
  OP_f64_store = 0x39,
  OP_i32_store8 = 0x3a,
  OP_i32_store16 = 0x3b,
  OP_i64_store8 = 0x3c,
  OP_i64_store16 = 0x3d,
  OP_i64_store32 = 0x3e,
  OP_current_memory = 0x3f,
  OP_grow_memory = 0x40,

  // Constants
  OP_i32_const = 0x41,
  OP_i64_const = 0x42,
  OP_f32_const = 0x43,
  OP_f64_const = 0x44,

  // Comparison operators
  OP_i32_eqz = 0x45,
  OP_i32_eq = 0x46,
  OP_i32_ne = 0x47,
  OP_i32_lt_s = 0x48,
  OP_i32_lt_u = 0x49,
  OP_i32_gt_s = 0x4a,
  OP_i32_gt_u = 0x4b,
  OP_i32_le_s = 0x4c,
  OP_i32_le_u = 0x4d,
  OP_i32_ge_s = 0x4e,
  OP_i32_ge_u = 0x4f,
  OP_i64_eqz = 0x50,
  OP_i64_eq = 0x51,
  OP_i64_ne = 0x52,
  OP_i64_lt_s = 0x53,
  OP_i64_lt_u = 0x54,
  OP_i64_gt_s = 0x55,
  OP_i64_gt_u = 0x56,
  OP_i64_le_s = 0x57,
  OP_i64_le_u = 0x58,
  OP_i64_ge_s = 0x59,
  OP_i64_ge_u = 0x5a,
  OP_f32_eq = 0x5b,
  OP_f32_ne = 0x5c,
  OP_f32_lt = 0x5d,
  OP_f32_gt = 0x5e,
  OP_f32_le = 0x5f,
  OP_f32_ge = 0x60,
  OP_f64_eq = 0x61,
  OP_f64_ne = 0x62,
  OP_f64_lt = 0x63,
  OP_f64_gt = 0x64,
  OP_f64_le = 0x65,
  OP_f64_ge = 0x66,

  // Numeric operators
  OP_i32_clz = 0x67,
  OP_i32_ctz = 0x68,
  OP_i32_popcnt = 0x69,
  OP_i32_add = 0x6a,
  OP_i32_sub = 0x6b,
  OP_i32_mul = 0x6c,
  OP_i32_div_s = 0x6d,
  OP_i32_div_u = 0x6e,
  OP_i32_rem_s = 0x6f,
  OP_i32_rem_u = 0x70,
  OP_i32_and = 0x71,
  OP_i32_or = 0x72,
  OP_i32_xor = 0x73,
  OP_i32_shl = 0x74,
  OP_i32_shr_s = 0x75,
  OP_i32_shr_u = 0x76,
  OP_i32_rotl = 0x77,
  OP_i32_rotr = 0x78,
  OP_i64_clz = 0x79,
  OP_i64_ctz = 0x7a,
  OP_i64_popcnt = 0x7b,
  OP_i64_add = 0x7c,
  OP_i64_sub = 0x7d,
  OP_i64_mul = 0x7e,
  OP_i64_div_s = 0x7f,
  OP_i64_div_u = 0x80,
  OP_i64_rem_s = 0x81,
  OP_i64_rem_u = 0x82,
  OP_i64_and = 0x83,
  OP_i64_or = 0x84,
  OP_i64_xor = 0x85,
  OP_i64_shl = 0x86,
  OP_i64_shr_s = 0x87,
  OP_i64_shr_u = 0x88,
  OP_i64_rotl = 0x89,
  OP_i64_rotr = 0x8a,
  OP_f32_abs = 0x8b,
  OP_f32_neg = 0x8c,
  OP_f32_ceil = 0x8d,
  OP_f32_floor = 0x8e,
  OP_f32_trunc = 0x8f,
  OP_f32_nearest = 0x90,
  OP_f32_sqrt = 0x91,
  OP_f32_add = 0x92,
  OP_f32_sub = 0x93,
  OP_f32_mul = 0x94,
  OP_f32_div = 0x95,
  OP_f32_min = 0x96,
  OP_f32_max = 0x97,
  OP_f32_copysign = 0x98,
  OP_f64_abs = 0x99,
  OP_f64_neg = 0x9a,
  OP_f64_ceil = 0x9b,
  OP_f64_floor = 0x9c,
  OP_f64_trunc = 0x9d,
  OP_f64_nearest = 0x9e,
  OP_f64_sqrt = 0x9f,
  OP_f64_add = 0xa0,
  OP_f64_sub = 0xa1,
  OP_f64_mul = 0xa2,
  OP_f64_div = 0xa3,
  OP_f64_min = 0xa4,
  OP_f64_max = 0xa5,
  OP_f64_copysign = 0xa6,

  // Conversions
  OP_i32_wrap_i64 = 0xa7,
  OP_i32_trunc_s_f32 = 0xa8,
  OP_i32_trunc_u_f32 = 0xa9,
  OP_i32_trunc_s_f64 = 0xaa,
  OP_i32_trunc_u_f64 = 0xab,
  OP_i64_extend_s_i32 = 0xac,
  OP_i64_extend_u_i32 = 0xad,
  OP_i64_trunc_s_f32 = 0xae,
  OP_i64_trunc_u_f32 = 0xaf,
  OP_i64_trunc_s_f64 = 0xb0,
  OP_i64_trunc_u_f64 = 0xb1,
  OP_f32_convert_s_i32 = 0xb2,
  OP_f32_convert_u_i32 = 0xb3,
  OP_f32_convert_s_i64 = 0xb4,
  OP_f32_convert_u_i64 = 0xb5,
  OP_f32_demote_f64 = 0xb6,
  OP_f64_convert_s_i32 = 0xb7,
  OP_f64_convert_u_i32 = 0xb8,
  OP_f64_convert_s_i64 = 0xb9,
  OP_f64_convert_u_i64 = 0xba,
  OP_f64_promote_f32 = 0xbb,

  // Reinterpretations
  OP_i32_reinterpret_f32 = 0xbc,
  OP_i64_reinterpret_f64 = 0xbd,
  OP_f32_reinterpret_i32 = 0xbe,
  OP_f64_reinterpret_i64 = 0xbf,
};

enum ERROR_CODE
{
  ERR_SUCCESS = 0,

  // Parse errors that immediately terminate parsing
  ERR_PARSE_UNEXPECTED_EOF = -0x01,
  ERR_PARSE_INVALID_MAGIC_COOKIE = -0x02,
  ERR_PARSE_INVALID_VERSION = -0x03,
  ERR_PARSE_INVALID_FILE_LENGTH = -0x04,
  ERR_PARSE_INVALID_NAME = -0x05,

  // Fatal errors that prevent properly parsing the module
  ERR_FATAL_INVALID_SECTION_ORDER = -0x10,
  ERR_FATAL_UNKNOWN_KIND = -0x11,
  ERR_FATAL_EXPECTED_END_INSTRUCTION = -0x12,
  ERR_FATAL_UNKNOWN_INSTRUCTION = -0x13,
  ERR_FATAL_BAD_ELEMENT_TYPE = -0x14,
  ERR_FATAL_OUT_OF_MEMORY = -0x15,
  ERR_FATAL_UNKNOWN_SECTION = -0x16,
  ERR_FATAL_INVALID_ENCODING = -0x17,
  ERR_FATAL_UNKNOWN_FUNCTION_SIGNATURE = -0x18,
  ERR_FATAL_BAD_HASH = -0x19,
  ERR_FATAL_DUPLICATE_EXPORT = -0x1A,
  ERR_FATAL_NULL_POINTER = -0x1B,
  ERR_FATAL_UNKNOWN_TARGET = -0x1C,
  ERR_FATAL_FILE_ERROR = -0x1D,
  ERR_FATAL_INVALID_MODULE = -0x1E,
  ERR_FATAL_LINK_ERROR = -0x1F,

  // Validation errors that prevent compiling the module
  ERR_VALIDATION_ERROR = -0x100,
  ERR_INVALID_FUNCTION_INDEX = -0x101,
  ERR_INVALID_TABLE_INDEX = -0x102,
  ERR_INVALID_MEMORY_INDEX = -0x103,
  ERR_INVALID_GLOBAL_INDEX = -0x104,
  ERR_UNKNOWN_SIGNATURE_TYPE = -0x105,
  ERR_MULTIPLE_RETURN_VALUES = -0x106,
  ERR_INVALID_IDENTIFIER = -0x107,
  ERR_MUTABLE_GLOBAL = -0x108,
  ERR_INVALID_IMPORT_MEMORY_MINIMUM = -0x109,
  ERR_INVALID_IMPORT_MEMORY_MAXIMUM = -0x10A,
  ERR_INVALID_IMPORT_TABLE_MINIMUM = -0x10B,
  ERR_INVALID_IMPORT_TABLE_MAXIMUM = -0x10C,
  ERR_UNKNOWN_MODULE = -0x10D,
  ERR_UNKNOWN_EXPORT = -0x10E,
  ERR_IMPORT_EXPORT_MISMATCH = -0x10F,
  ERR_INVALID_TYPE_INDEX = -0x110,
  ERR_FUNCTION_BODY_MISMATCH = -0x111,
  ERR_INVALID_TABLE_ELEMENT_TYPE = -0x112,
  ERR_INVALID_LIMITS = -0x113,
  ERR_INVALID_INITIALIZER = -0x114,
  ERR_INVALID_GLOBAL_TYPE = -0x115,
  ERR_INVALID_TABLE_TYPE = -0x116,
  ERR_INVALID_MEMORY_TYPE = -0x117,
  ERR_INVALID_START_FUNCTION = -0x118,
  ERR_INVALID_TABLE_OFFSET = -0x119,
  ERR_INVALID_MEMORY_OFFSET = -0x11A,
  ERR_INVALID_FUNCTION_BODY = -0x11B,
  ERR_INVALID_VALUE_STACK = -0x11C,
  ERR_INVALID_TYPE = -0x11D,
  ERR_EXPECTED_ELSE_INSTRUCTION = -0x11E,
  ERR_INVALID_BRANCH_TYPE = -0x11F,
  ERR_IF_ELSE_MISMATCH = -0x120,
  ERR_END_MISMATCH = -0x121,
  ERR_SIGNATURE_MISMATCH = -0x122,
  ERR_INVALID_BRANCH_DEPTH = -0x123,
  ERR_INVALID_LOCAL_INDEX = -0x124,
  ERR_INVALID_ARGUMENT_TYPE = -0x125,
  ERR_MULTIPLE_ENTRY_POINTS = -0x126,
  ERR_INVALID_BLOCK_SIGNATURE = -0x127,
  ERR_INVALID_MEMORY_ALIGNMENT = -0x128,
  ERR_INVALID_RESERVED_VALUE = -0x129,
};

enum ENVIRONMENT_FLAGS
{
  ENV_STRICT = (1 << 0),
  ENV_MULTITHREADED = (1 << 1),
  ENV_DEBUG = (1 << 2),
  ENV_DLL = (1 << 3),
  ENV_OPTIMIZE_INLINE = (1 << 4),
  ENV_OPTIMIZE_ANALYSIS = (1 << 5),
  ENV_OPTIMIZE_VECTORIZE = (1 << 6),
  ENV_OPTIMIZE_ALL = ENV_OPTIMIZE_INLINE| ENV_OPTIMIZE_ANALYSIS| ENV_OPTIMIZE_VECTORIZE,
};

typedef struct __BYTE_ARRAY
{
  varuint32 n_bytes;
  uint8_t* bytes;
} ByteArray;

typedef ByteArray Identifier;

typedef union __IMMEDIATE
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
  memflags _memflags;
  varuptr _varuptr;
  struct { varuint32 n_table; varuint32* table; };
} Immediate;

typedef struct __INSTRUCTION
{
  byte opcode;
  Immediate immediates[MAX_IMMEDIATES];
} Instruction;

typedef struct __FUNC_SIG
{
  varsint7 form;
  varsint7* params;
  varuint32 n_params;
  varsint7* returns;
  varuint32 n_returns;
} FunctionSig;

typedef struct __RESIZABLE_LIMITS
{
  varuint32 flags;
  varuint32 minimum;
  varuint32 maximum;
} ResizableLimits;

typedef struct __MEMORY_DESC
{
  ResizableLimits limits;
} MemoryDesc;

typedef struct __TABLE_DESC
{
  varsint7 element_type;
  ResizableLimits resizable;
} TableDesc;

typedef struct __GLOBAL_DESC
{
  varsint7 type;
  varuint1 mutability;
} GlobalDesc;

typedef struct __GLOBAL_DECL
{
  GlobalDesc desc;
  Instruction init;
} GlobalDecl;

typedef struct __FUNCTION_DESC
{
  varuint32 sig_index;
  Identifier debug_name;
  const char** local_names;
} FunctionDesc;

typedef struct __IMPORT
{
  Identifier module_name;
  Identifier export_name;
  varuint7 kind; // WA_KIND
  union
  {
    FunctionDesc func_desc;
    TableDesc table_desc;
    MemoryDesc mem_desc;
    GlobalDesc global_desc;
  };
} Import;

typedef struct __EXPORT
{
  Identifier name;
  varuint7 kind; // WA_KIND
  varuint32 index;
} Export;

typedef struct __TABLE_INIT
{
  varuint32 index;
  Instruction offset;
  varuint32 n_elems;
  varuint32* elems;
} TableInit;

typedef struct __LOCAL_ENTRY
{
  varuint32 count;
  varuint7 type;
} LocalEntry;

typedef struct __FUNCTION_BODY
{
  varuint32 body_size;
  varuint32 n_locals;
  LocalEntry* locals;
  Instruction* body;
  varuint32 n_body; // INTERNAL: track actual number of instructions
  Identifier debug_name; // INTERNAL: debug name if it exists
  const char** local_names; // INTERNAL: debug names of locals, if known from name section. Always size of n_locals and NULL if doesn't exist.
} FunctionBody;

typedef struct __DATA_INIT
{
  varuint32 index;
  Instruction offset;
  ByteArray data;
} DataInit;

typedef struct __CUSTOM_SECTION
{
  varuint7 opcode;
  varuint32 payload;
  Identifier name; // The first thing in a custom section must always be a legal identifier
  uint8_t* data;
} CustomSection;

typedef struct __MODULE
{
  uint32 magic_cookie;
  uint32 version;
  uint32_t knownsections; // bit-index corresponds to that OPCODE section being loaded
  Identifier name; // Name of the module as determined by the environment or name section

  struct TypeSection
  {
    varuint32 n_functions;
    FunctionSig* functions;
  } type;

  struct ImportSection
  {
    varuint32 functions;
    varuint32 tables;
    varuint32 memory;
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
    varuint32 n_memory;
    MemoryDesc* memory;
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
} Module;

typedef struct __VALIDATION_ERROR
{
  int code;
  char* error;
  Module* m;
  struct __VALIDATION_ERROR* next;
} ValidationError;

typedef struct __EMBEDDING
{
  void* data;
  uint64_t size; // If size is 0, data points to a null terminated UTF8 file path
  int tag; // defines the type of embedding data included, determined by the runtime. 0 is always a static library file for the current platform.
  struct __EMBEDDING* next;
} Embedding;

typedef struct __ENVIRONMENT
{
  size_t n_modules; // number of completely loaded modules (for multithreading)
  size_t size; // Size of loaded or loading modules
  size_t capacity; // Capacity of the modules array
  Module* modules;
  Embedding* embeddings;
  ValidationError* errors; //A linked list of non-fatal validation errors that prevent proper execution.
  uint64_t flags;
  unsigned int maxthreads; // Max number of threads for any multithreaded action. If 0, there is no limit.

  struct kh_modules_s* modulemap;
} Environment;

#ifdef  __cplusplus
}
#endif

#endif