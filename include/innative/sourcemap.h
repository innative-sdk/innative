// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__SOURCE_MAP_H
#define IN__SOURCE_MAP_H

#include <stddef.h>
#include <stdbool.h>
#include "innative/innative.h"
#include "innative/errors.h"

#ifdef __cplusplus
extern "C" {
#endif

struct IN_WASM_ENVIRONMENT;

typedef struct IN_SOURCE_SEGMENT
{
  unsigned long long linecolumn; // column is lowest 32 bits, line is highest 32 bits
  size_t source_index;
  unsigned int original_line;
  unsigned int original_column;
  size_t name_index;
} SourceMapSegment;

typedef struct IN_SOURCE_ENUMERATOR
{
  size_t name_index;
  unsigned long long val;
} SourceMapEnum;

enum
{
  IN_SOURCE_TYPE_PUBLIC       = 0b01,
  IN_SOURCE_TYPE_PRIVATE      = 0b10,
  IN_SOURCE_TYPE_PROTECTED    = 0b11,
  IN_SOURCE_TYPE_VIRTUAL      = 0b100,
  IN_SOURCE_TYPE_PURE_VIRTUAL = 0b1000,
  IN_SOURCE_TYPE_FRIEND       = 0b10000,
  IN_SOURCE_TYPE_ARTIFICIAL   = 0b100000,
  IN_SOURCE_TYPE_DECLARATION  = 0b1000000,
  IN_SOURCE_TYPE_DEFAULT      = 0b10000000,
  IN_SOURCE_TYPE_EXPORT       = 0b100000000,
  IN_SOURCE_TYPE_MUTABLE      = 0b1000000000,
  IN_SOURCE_TYPE_ENUM_CLASS   = 0b10000000000,
};

typedef struct IN_SOURCE_TYPE
{
  size_t offset;
  size_t source_index;
  unsigned int original_line;
  size_t n_types;
  unsigned short tag;
  unsigned short flags;
  size_t type_index; // Used for single types or a base type
  union
  {
    size_t* types; // Child type indexes
    size_t* enumerators;
    unsigned short encoding;   // For base types, this is the type encoding
    unsigned short bit_stride; // Used for arrays and some certain other types
    unsigned int bit_offset;
  };
  unsigned short bit_size;
  unsigned short byte_align;
  size_t name_index; // type name
} SourceMapType;

typedef struct IN_SOURCE_VARIABLE
{
  size_t offset;
  size_t source_index;
  unsigned int original_line;
  unsigned int original_column;
  size_t type_index;
  size_t name_index;
  unsigned short tag;
  size_t n_expr;
  long long* p_expr;
} SourceMapVariable;

// Can represent a lexical scope or an inlined subroutine scope
typedef struct IN_SOURCE_RANGE
{
  size_t scope;
  size_t low;
  size_t high;
} SourceMapRange;

typedef struct IN_SOURCE_LOCATION
{
  SourceMapRange range;
  size_t n_expr;
  long long* p_expr;
} SourceMapLocation;

typedef struct IN_SOURCE_SCOPE
{
  size_t name_index;
  size_t n_variables;
  size_t* variables; // Contains either variables or formal parameters (for functions)
} SourceMapScope;

typedef struct IN_SOURCE_FUNCTION
{
  SourceMapRange range;
  size_t source_index;
  unsigned int original_line;
  size_t type_index;
} SourceMapFunction;

typedef struct IN_SOURCE_MAP
{
  short version;
  const char* file;
  const char* sourceRoot;
  const char** sources;
  size_t n_sources;
  const char** sourcesContent;
  size_t n_sourcesContent;
  const char** names;
  size_t n_names;
  struct IN_SOURCE_SEGMENT* segments;
  size_t n_segments;
  size_t x_google_linecount;
  struct IN_SOURCE_TYPE* x_innative_types;
  size_t n_innative_types;
  struct IN_SOURCE_VARIABLE* x_innative_variables;
  size_t n_innative_variables;
  struct IN_SOURCE_LOCATION* x_innative_locations;
  size_t n_innative_locations;
  struct IN_SOURCE_RANGE* x_innative_ranges;
  size_t n_innative_ranges;
  struct IN_SOURCE_SCOPE* x_innative_scopes;
  size_t n_innative_scopes;
  struct IN_SOURCE_FUNCTION* x_innative_functions;
  size_t n_innative_functions;
  struct IN_SOURCE_ENUMERATOR* x_innative_enumerators;
  size_t n_innative_enumerators;
  size_t* x_innative_globals;
  size_t n_innative_globals;
} SourceMap;

extern IN_COMPILER_DLLEXPORT enum IN_ERROR ParseDWARF(struct IN_WASM_ENVIRONMENT* env, SourceMap* map, const char* obj,
                                                      size_t len);
enum IN_ERROR ParseSourceMap(const struct IN_WASM_ENVIRONMENT* env, SourceMap* map, const char* data, size_t len);
enum IN_ERROR SerializeSourceMap(const SourceMap* map, const char* out);
enum IN_ERROR DumpSourceMap(const SourceMap* map, const char* out);

#ifdef __cplusplus
}
#endif

#endif
