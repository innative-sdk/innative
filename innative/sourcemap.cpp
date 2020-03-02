// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/sourcemap.h"
#include "constants.h"
#include "utility.h"
#include <algorithm>
#include <fstream>

namespace innative {
  namespace sourcemap {
    typedef IN_ERROR (*fnParseObject)(const Environment& env, SourceMap* map, const char* keybegin, const char* keyend,
                                      const char*& data, const char* end);

    int32_t DecodeVLQ(const char*& data);
    void SkipWhitespace(const char*& data, const char* end);
    const char* ParseKey(const char* data, const char*& end);
    int64_t ParseNumber(const char*& data, const char* end);
    const char* ParseString(const Environment& env, const char*& data, const char* end);
    IN_ERROR ParseObject(const Environment& env, SourceMap* map, const char*& data, const char* end, fnParseObject parse);
    IN_ERROR ParseRoot(const Environment& env, SourceMap* map, const char* keybegin, const char* keyend, const char*& data,
                       const char* end);
    template<class T>
    IN_ERROR ParseArrayInner(const Environment& env, const char*& data, const char* end, T*& out, size_t& count,
                             IN_ERROR (*f)(const Environment& env, const char*& data, const char* end, T& result));
    template<class T>
    IN_ERROR ParseArray(const Environment& env, const char*& data, const char* end, T*& out, size_t& count,
                        IN_ERROR (*f)(const Environment& env, const char*& data, const char* end, T& result));
    IN_ERROR ParseArrayString(const Environment& env, const char*& data, const char* end, const char*& result);
    IN_ERROR ParseMapping(const Environment& env, SourceMap* map, const char*& data, const char* end);

    template<class T> void Serialize(T t, FILE* f);
    template<class T> void SerializeKeyValue(const char* key, T value, FILE* f);
    void EncodeVLQ(int32_t i, FILE* f);
    void SerializeMapping(const SourceMap* map, FILE* f);
  }
}

using namespace innative;

int32_t sourcemap::DecodeVLQ(const char*& data)
{
  uint32_t value = 0; // must be unsigned so we get the correct bitshift behavior
  uint8_t digit;
  uint32_t offset = 0;

  do
  {
    if(data[0] > 'z')
      return 0;
    digit = utility::BASE64[data[0]];
    if(digit == 255) // If we get an illegal encoding, it's important to exit BEFORE incrementing data
      return 0;
    ++data;
    value += (digit & (~utility::VLQ_CONTINUATION_BIT)) << offset;
    offset += 5;
  } while(digit & utility::VLQ_CONTINUATION_BIT);

  return (value & 1) ? -static_cast<int32_t>(value >> 1) : (value >> 1);
}

void sourcemap::SkipWhitespace(const char*& data, const char* end)
{
  while(data < end && isspace(*data))
    ++data;
}

const char* sourcemap::ParseKey(const char* data, const char*& end)
{
  SkipWhitespace(data, end);
  if(data >= end || *data != '"')
    return nullptr;

  const char* cur = ++data;
  while(cur < end && *cur != '"')
    ++cur;

  if(cur >= end || *cur != '"')
    return nullptr;
  end = cur;
  return data;
}

int64_t sourcemap::ParseNumber(const char*& data, const char* end)
{
  char buf[32];
  int i = 0;
  while((isdigit(*data) || *data == '-') && i < 31)
    buf[i++] = *data++;
  buf[i] = 0;
  return strtoll(buf, 0, 10);
}

const char* sourcemap::ParseString(const Environment& env, const char*& data, const char* end)
{
  SkipWhitespace(data, end);
  if(data >= end || *data != '"')
    return nullptr;
  const char* cur = ++data;
  while(cur < end && (cur[0] != '"' || cur[-1] == '\\'))
    ++cur;
  if(cur >= end || *cur != '"')
    return nullptr;

  ptrdiff_t len = cur - data;
  ptrdiff_t i   = 0;
  char* s       = utility::tmalloc<char>(env, len + 1);
  if(!s)
    return nullptr;

  for(; data < cur; ++i)
  {
    if(*data == '\\')
    {
      switch(*++data)
      {
      case 'b': s[i] = '\b'; break;
      case 'f': s[i] = '\f'; break;
      case 'n': s[i] = '\n'; break;
      case 'r': s[i] = '\r'; break;
      case 't': s[i] = '\t'; break;
      case '"': s[i] = '\"'; break;
      case '\'': s[i] = '\''; break;
      case '\\': s[i] = '\\'; break;
      }
    }
    else
      s[i] = *data;
    ++data;
  }

  ++data; // skip last '"'
  s[i] = 0;
  return s;
}

// We use recursion instead of a loop so we can build up the results in the stack, then allocate an array exactly once. This
// is done because we can't count array entries without decoding them, because they might be strings.
template<class T>
IN_ERROR sourcemap::ParseArrayInner(const Environment& env, const char*& data, const char* end, T*& out, size_t& count,
                                    IN_ERROR (*f)(const Environment& env, const char*& data, const char* end, T& result))
{
  IN_ERROR err = ERR_SUCCESS;
  if(data < end && *data != ']')
  {
    ++data;

    T result;
    IN_ERROR err = f(env, data, end, result);
    if(err < 0)
      return err;

    SkipWhitespace(data, end);
    if(data >= end || (*data != ']' && *data != ','))
      return ERR_MAP_UNEXPECTED_END;
    SkipWhitespace(data, end);

    size_t i = count;
    err      = ParseArrayInner(env, data, end, out, ++count, f);
    if(err == ERR_SUCCESS)
      out[i] = result;
  }
  else if(!(out = utility::tmalloc<T>(env, count)))
    err = ERR_FATAL_OUT_OF_MEMORY;
  return err;
}

template<class T>
IN_ERROR sourcemap::ParseArray(const Environment& env, const char*& data, const char* end, T*& out, size_t& count,
                               IN_ERROR (*f)(const Environment& env, const char*& data, const char* end, T& result))
{
  SkipWhitespace(data, end);
  if(data >= end || *data != '[')
  {
    if(data + 4 > end || STRNICMP(data, "null", 4) != 0)
      return ERR_MAP_EXPECTED_OPEN_BRACKET;
    data += 4;
    out   = 0;
    count = 0;
    return ERR_SUCCESS;
  }

  if(data + 1 >= end)
    return ERR_MAP_EXPECTED_CLOSE_BRACKET;

  count = 0;
  if(data[1] != ']')
  {
    IN_ERROR err = ParseArrayInner(env, data, end, out, count, f);
    if(err < 0)
      return err;

    SkipWhitespace(data, end);
    if(data >= end || *data != ']')
      return ERR_MAP_EXPECTED_CLOSE_BRACKET;
  }
  else
  {
    out = nullptr;
    ++data;
  }

  ++data;
  return ERR_SUCCESS;
}

IN_ERROR sourcemap::ParseObject(const Environment& env, SourceMap* map, const char*& data, const char* end,
                                fnParseObject parse)
{
  SkipWhitespace(data, end);
  if(data >= end || *data != '{')
    return ERR_MAP_EXPECTED_OPEN_BRACE;
  ++data;

  while(data < end && *data != '}')
  {
    const char* keyend = end;
    const char* key    = ParseKey(data, keyend);
    data               = keyend + 1;
    SkipWhitespace(data, end);

    if(data >= end || *data != ':')
      return ERR_MAP_EXPECTED_COLON;
    ++data;

    IN_ERROR err = (*parse)(env, map, key, keyend, data, end);
    if(err < 0)
      return err;

    SkipWhitespace(data, end);
    if(data < end && *data == ',')
      ++data;
    else if(data >= end || *data != '}')
      return ERR_MAP_EXPECTED_CLOSE_BRACE;
    SkipWhitespace(data, end);
  }

  if(data >= end || *data != '}')
    return ERR_MAP_EXPECTED_CLOSE_BRACE;
  return ERR_SUCCESS;
}

IN_ERROR sourcemap::ParseArrayString(const Environment& env, const char*& data, const char* end, const char*& result)
{
  result = ParseString(env, data, end);
  return !result ? ERR_MAP_INVALID_STRING : ERR_SUCCESS;
}

IN_ERROR sourcemap::ParseMapping(const Environment& env, SourceMap* map, const char*& data, const char* end)
{
  SkipWhitespace(data, end);
  if(data >= end || *data != '"')
    return ERR_MAP_EXPECTED_QUOTE;
  const char* cur = ++data;
  if(data >= end || *data == '"')
  {
    ++data;
    return ERR_SUCCESS; // If the string is empty, do nothing
  }

  // In this case, our string has a well-defined format, so we can simply count commas and semicolons. Trying to use a
  // recursion stack trick here is ill-advised, because the mapping string can be incredibly huge for large files.
  size_t n = 1;

  // We iterate through the string and count both semicolons and commas, which gives us a segment count
  while(cur < end && *cur != '"')
  {
    if(*cur == ';' || *cur == ',')
      ++n;
    ++cur;
  }
  if(cur >= end || *cur != '"')
    return ERR_MAP_EXPECTED_QUOTE;

  map->n_segments = n;
  map->segments   = utility::tmalloc<SourceMapSegment>(env, n);
  if(!map->segments)
    return ERR_FATAL_OUT_OF_MEMORY;

  // Then we iterate forwards again, assigning values to each subarray element.
  assert(!n);
  uint64_t line                = 0;
  n                            = 0;
  size_t last_column           = 0;
  size_t last_source_index     = 0;
  size_t last_original_line    = 1;
  size_t last_original_column  = 0;
  size_t last_name_index       = 0;
  SourceMapSegment* last_valid = 0;

  while(*data != '"') // we can drop the cur < end check because we know it has to be a valid string
  {
    if(n >= map->n_segments)
      return ERR_MAP_UNEXPECTED_BASE64;

    map->segments[n].linecolumn   = (line << 32) | (last_column += DecodeVLQ(data));
    map->segments[n].source_index = last_source_index += DecodeVLQ(data);
    map->segments[n].original_line =
      static_cast<decltype(map->segments[n].original_line)>(last_original_line += DecodeVLQ(data));
    map->segments[n].original_column =
      static_cast<decltype(map->segments[n].original_column)>(last_original_column += DecodeVLQ(data));
    map->segments[n].name_index = last_name_index += DecodeVLQ(data);

    if(data >= end)
      return ERR_MAP_UNEXPECTED_END;
    else if(*data == ',')
      ++data;
    else if(*data == ';')
    {
      last_column = 0;
      ++line;
      ++data;
    }
    else if(*data != '"')
      return ERR_MAP_UNEXPECTED_BASE64;

    ++n;
  }

  map->n_segments = n;
  std::sort(map->segments, map->segments + map->n_segments,
            [](SourceMapSegment& a, SourceMapSegment& b) { return a.linecolumn < b.linecolumn; });

  ++data;
  return ERR_SUCCESS;
}

IN_ERROR sourcemap::ParseRoot(const Environment& env, SourceMap* map, const char* keybegin, const char* keyend,
                              const char*& data, const char* end)
{
  if(!STRNICMP(keybegin, "version", keyend - keybegin))
    map->version = static_cast<decltype(map->version)>(ParseNumber(data, end));
  else if(!STRNICMP(keybegin, "file", keyend - keybegin))
    map->file = ParseString(env, data, end);
  else if(!STRNICMP(keybegin, "sourceRoot", keyend - keybegin))
    map->sourceRoot = ParseString(env, data, end);
  else if(!STRNICMP(keybegin, "sources", keyend - keybegin))
    return ParseArray<const char*>(env, data, end, map->sources, map->n_sources, ParseArrayString);
  else if(!STRNICMP(keybegin, "sourcesContent", keyend - keybegin) || // generators sure seem confused about this key
          !STRNICMP(keybegin, "sourceContent", keyend - keybegin) ||
          !STRNICMP(keybegin, "sourceContents", keyend - keybegin))
    return ParseArray<const char*>(env, data, end, map->sourcesContent, map->n_sourcesContent, ParseArrayString);
  else if(!STRNICMP(keybegin, "names", keyend - keybegin))
    return ParseArray<const char*>(env, data, end, map->names, map->n_names, ParseArrayString);
  else if(!STRNICMP(keybegin, "mappings", keyend - keybegin))
    return ParseMapping(env, map, data, end);
  else if(!STRNICMP(keybegin, "x_google_linecount", keyend - keybegin))
    map->x_google_linecount = static_cast<decltype(map->x_google_linecount)>(ParseNumber(data, end));
  else
    return ERR_MAP_UNKNOWN_KEY;

  return ERR_SUCCESS;
}

IN_ERROR ParseSourceMap(const Environment* env, SourceMap* map, const char* data, size_t len)
{
  std::unique_ptr<uint8_t[]> f;
  const char* end;
  if(!len)
  {
    size_t sz;
    f = utility::LoadFile(utility::GetPath(data), sz);
    if(!f)
      return ERR_FATAL_FILE_ERROR;
    data = reinterpret_cast<char*>(f.get());
    end  = data + sz;
  }
  else
    end = data + len;

  *map = { 0 };
  return sourcemap::ParseObject(*env, map, data, end, &sourcemap::ParseRoot);
}

template<> void sourcemap::Serialize<size_t>(size_t s, FILE* f) { fprintf(f, "%zu", s); }
template<> void sourcemap::Serialize<const char*>(const char* s, FILE* f)
{
  fputc('"', f);
  if(s)
  {
    const char* cur = s;
    while(*cur)
    {
      char inject = 0;
      switch(*cur)
      {
      case '\b': inject = 'b'; break;
      case '\f': inject = 'f'; break;
      case '\n': inject = 'n'; break;
      case '\r': inject = 'r'; break;
      case '\t': inject = 't'; break;
      case '\"': inject = '"'; break;
      case '\'': inject = '\''; break;
      case '\\': inject = '\\'; break;
      }

      if(inject)
      {
        if(cur > s)
          fwrite(s, 1, cur - s, f);
        s = cur + 1;
        fputc('\\', f);
        fputc(inject, f);
      }

      ++cur;
    }

    if(cur > s)
      fwrite(s, 1, cur - s, f);
  }
  fputc('"', f);
}

template<> void sourcemap::Serialize<std::pair<const char**, size_t>>(std::pair<const char**, size_t> s, FILE* f)
{
  fputc('[', f);

  for(size_t i = 0; i < s.second; ++i)
  {
    if(i > 0)
      fputc(',', f);
    Serialize(s.first[i], f);
  }

  fputc(']', f);
}

template<class T> void sourcemap::SerializeKeyValue(const char* key, T value, FILE* f)
{
  Serialize(key, f);
  fputc(':', f);
  Serialize<T>(value, f);
}

void sourcemap::EncodeVLQ(int32_t i, FILE* f)
{
  char negative      = i < 0;
  uint32_t remaining = abs(i);

  // First byte contains the sign bit and must be done seperately
  char value = negative | ((remaining & 0b1111) << 1);
  remaining &= ~0b1111;

  uint32_t offset = 4;
  while(remaining)
  {
    value |= utility::VLQ_CONTINUATION_BIT;
    fputc(utility::IN_BASE64[value], f);

    uint32_t mask = (0b11111 << offset);
    value         = (remaining & mask) >> offset;
    remaining &= ~mask;
    offset += 5;
  }

  fputc(utility::IN_BASE64[value], f);
}

void sourcemap::SerializeMapping(const SourceMap* map, FILE* f)
{
  sourcemap::Serialize("mappings", f);
  fputc(':', f);
  fputc('"', f);
  uint32_t last_line          = 0;
  uint32_t last_column        = 0;
  size_t last_source_index    = 0;
  size_t last_original_line   = 0;
  size_t last_original_column = 0;
  size_t last_name_index      = 0;
  bool comma                  = false;
  for(size_t i = 0; i < map->n_segments; ++i)
  {
    uint32_t line = map->segments[i].linecolumn >> 32;
    while(last_line < line)
    {
      fputc(';', f);
      last_column = 0;
      comma       = false;
      ++last_line;
    }

    if(comma)
      fputc(',', f);
    comma = true;

    int32_t diff_column          = (map->segments[i].linecolumn & 0xFFFFFFFF) - last_column;
    int32_t diff_source_index    = static_cast<int32_t>(map->segments[i].source_index - last_source_index);
    int32_t diff_original_line   = static_cast<int32_t>(map->segments[i].original_line - 1 - last_original_line);
    int32_t diff_original_column = static_cast<int32_t>(map->segments[i].original_column - last_original_column);
    int32_t diff_name_index      = static_cast<int32_t>(map->segments[i].name_index - last_name_index);
    last_column                  = (map->segments[i].linecolumn & 0xFFFFFFFF);
    last_source_index            = map->segments[i].source_index;
    last_original_line           = map->segments[i].original_line - 1;
    last_original_column         = map->segments[i].original_column;
    last_name_index              = map->segments[i].name_index;

    EncodeVLQ(diff_column, f);
    EncodeVLQ(diff_source_index, f);
    EncodeVLQ(diff_original_line, f);
    EncodeVLQ(diff_original_column, f);

    if(diff_name_index)
      EncodeVLQ(diff_name_index, f);
  }
  fputc('"', f);
}

enum IN_ERROR SerializeSourceMap(const SourceMap* map, const char* out)
{
  if(!map || !out)
    return ERR_FATAL_NULL_POINTER;
  path file = u8path(out);
  FILE* f;
  FOPEN(f, file.c_str(), "wb");
  if(!f)
    return ERR_FATAL_FILE_ERROR;

  fputc('{', f);
  sourcemap::SerializeKeyValue<size_t>("version", map->version, f);
  fputc(',', f);
  sourcemap::SerializeKeyValue("sources", std::pair<const char**, size_t>{ map->sources, map->n_sources }, f);
  fputc(',', f);
  sourcemap::SerializeKeyValue("names", std::pair<const char**, size_t>{ map->names, map->n_names }, f);
  fputc(',', f);
  sourcemap::SerializeMapping(map, f);
  fputc(',', f);
  sourcemap::SerializeKeyValue("sourceRoot", map->sourceRoot, f);
  fputc(',', f);
  sourcemap::SerializeKeyValue("sourcesContent",
                               std::pair<const char**, size_t>{ map->sourcesContent, map->n_sourcesContent }, f);

  if(map->file && map->file[0])
  {
    fputc(',', f);
    sourcemap::SerializeKeyValue("file", map->file, f);
  }

  if(map->x_google_linecount)
  {
    fputc(',', f);
    sourcemap::SerializeKeyValue<size_t>("x_google_linecount", map->x_google_linecount, f);
  }

  fputc('}', f);

  if(fclose(f) != 0)
    return ERR_FATAL_FILE_ERROR;
  return ERR_SUCCESS;
}

enum IN_ERROR DumpSourceMap(const SourceMap* map, const char* out)
{
  // This dumps the mapping and variable sections of the sourcemap for easier debugging
  std::ofstream fs(out, std::ios_base::trunc | std::ios_base::binary | std::ios_base::out);
  if(!fs)
    return ERR_FATAL_FILE_ERROR;

  fs << "Mapping: \n";

  for(size_t i = 0; i < map->n_segments; ++i)
  {
    auto& s = map->segments[i];
    fs << "\n[" << (s.linecolumn >> 32) + 1 << ':' << (s.linecolumn & 0xFFFFFFFF) << "] | "
       << ((s.source_index < map->n_sources) ? map->sources[s.source_index] : "(none)") << '[' << s.original_line << ':'
       << s.original_column << ']';
  }

  fs << "\n\nVariables: \n";

  for(size_t i = 0; i < map->n_innative_variables; ++i)
  {
    auto& v = map->x_innative_variables[i];
    fs << "\n"
       << ((v.name_index < map->n_names) ? map->names[v.name_index] : "") << "[" << map->x_innative_variables[i].tag << "] "
       << ((v.source_index < map->n_sources) ? map->sources[v.source_index] : "(none)") << '[' << v.original_line << ':'
       << v.original_column << ']';

    if(v.type_index < map->n_innative_types && map->x_innative_types[v.type_index].name_index < map->n_names)
      fs << ' ' << map->names[map->x_innative_types[v.type_index].name_index];
  }

  return ERR_SUCCESS;
}