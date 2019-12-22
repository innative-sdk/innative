// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__SOURCE_MAP_H
#define IN__SOURCE_MAP_H

#include <stddef.h>
#include "innative/innative.h"
#include "innative/errors.h"

struct IN_WASM_ENVIRONMENT;

typedef struct IN_SOURCE_SEGMENT
{
  size_t column;
  size_t source_index;
  size_t original_line;
  size_t original_column;
  size_t name_index;
} SourceMapSegment;

typedef struct IN_SOURCE_GROUP
{
  struct IN_SOURCE_SEGMENT* segments;
  size_t n_segments;
} SourceMapGroup;

typedef struct IN_SOURCE_TYPE
{
  struct IN_SOURCE_TYPE* p;
  size_t n;
} SourceMapType;

typedef struct IN_SOURCE_VARIABLE
{
  size_t type_index;
  size_t group;
  size_t segment;
  size_t name_index;
} SourceMapVariable;

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
  struct IN_SOURCE_GROUP* mappings; // One group per line
  size_t n_mappings;
  size_t x_google_linecount;
  struct IN_SOURCE_TYPE* x_innative_types;
  size_t n_innative_types;
  struct IN_SOURCE_VARIABLE* x_innative_variables;
  size_t n_innative_variables;
} SourceMap;

extern IN_COMPILER_DLLEXPORT enum IN_ERROR DWARFSourceMap(struct IN_WASM_ENVIRONMENT* env, SourceMap* map, const char* obj,
                                                          size_t len);
enum IN_ERROR ParseSourceMap(const struct IN_WASM_ENVIRONMENT* env, SourceMap* map, const char* data, size_t len);
enum IN_ERROR SerializeSourceMap(const SourceMap* map, const char* out);
const SourceMapSegment* GetSourceMapSegment(SourceMap* map, size_t line, size_t column);

#endif
