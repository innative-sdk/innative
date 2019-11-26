// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/sourcemap.h"
#include "innative/schema.h"
#include "util.h"
#include "stream.h"
#include "llvm/DebugInfo/DIContext.h"
#include "llvm/DebugInfo/DWARF/DWARFContext.h"
#include "llvm/Object/Archive.h"
#include "llvm/Object/MachOUniversal.h"
#include "llvm/Object/ObjectFile.h"

using namespace llvm;

typedef bool (*HandlerFn)(Environment* env, DWARFContext& DICtx, SourceMap* map, size_t code_section_offset);

static bool error(const Environment* env, llvm::StringRef Prefix, std::error_code EC)
{
  if(!EC)
    return true;
  fwrite(Prefix.begin(), 1, Prefix.size(), env->log);
  return false;
}

static bool handleBuffer(Environment* env, StringRef Filename, MemoryBufferRef Buffer, HandlerFn HandleObj, SourceMap* map);

static bool handleArchive(Environment* env, StringRef Filename, object::Archive& Arch, HandlerFn HandleObj, SourceMap* map)
{
  bool Result = true;
  Error Err   = Error::success();
  for(auto Child : Arch.children(Err))
  {
    auto BuffOrErr = Child.getMemoryBufferRef();
    if(!error(env, Filename, errorToErrorCode(BuffOrErr.takeError())))
      return false;
    auto NameOrErr = Child.getName();
    if(!error(env, Filename, errorToErrorCode(NameOrErr.takeError())))
      return false;
    std::string Name = (Filename + "(" + NameOrErr.get() + ")").str();
    Result &= handleBuffer(env, Name, BuffOrErr.get(), HandleObj, map);
  }
  return Result && error(env, Filename, errorToErrorCode(std::move(Err)));
}

static bool handleBuffer(Environment* env, StringRef Filename, MemoryBufferRef Buffer, HandlerFn HandleObj, SourceMap* map)
{
  size_t code_section_offset = 0;
  if(Buffer.getBufferSize() > 8)
  {
    innative::utility::Stream s = { (uint8_t*)Buffer.getBufferStart(), Buffer.getBufferSize(), 0 };

    IN_ERROR err = ERR_SUCCESS;
    if(s.ReadUInt32(err) == INNATIVE_WASM_MAGIC_COOKIE && s.ReadUInt32(err) == INNATIVE_WASM_MAGIC_VERSION)
    {
      while(!s.End())
      {
        varuint7 op = s.ReadVarUInt7(err);
        if(err < 0)
          break;

        if(op == WASM_SECTION_CODE)
        {
          code_section_offset = s.pos;
          break;
        }
        s.pos += s.ReadVarUInt32(err);

        if(err < 0)
          break;
      }
    }
  }

  Expected<std::unique_ptr<object::Binary>> BinOrErr = object::createBinary(Buffer);
  if(!error(env, Filename, errorToErrorCode(BinOrErr.takeError())))
    return false;

  bool Result = true;
  if(auto* Obj = dyn_cast<object::ObjectFile>(BinOrErr->get()))
  {
    std::unique_ptr<DWARFContext> DICtx = DWARFContext::create(*Obj);
    Result                              = HandleObj(env, *DICtx, map, code_section_offset);
  }
  else if(auto* Fat = dyn_cast<object::MachOUniversalBinary>(BinOrErr->get()))
    for(auto& ObjForArch : Fat->objects())
    {
      std::string ObjName = (Filename + "(" + ObjForArch.getArchFlagName() + ")").str();
      if(auto MachOOrErr = ObjForArch.getAsObjectFile())
      {
        auto& Obj                           = **MachOOrErr;
        std::unique_ptr<DWARFContext> DICtx = DWARFContext::create(Obj);
        Result &= HandleObj(env, *DICtx, map, code_section_offset);
        continue;
      }
      else
        consumeError(MachOOrErr.takeError());
      if(auto ArchiveOrErr = ObjForArch.getAsArchive())
      {
        if(!error(env, ObjName, errorToErrorCode(ArchiveOrErr.takeError())))
          return false;
        Result &= handleArchive(env, ObjName, *ArchiveOrErr.get(), HandleObj, map);
        continue;
      }
      else
        consumeError(ArchiveOrErr.takeError());
    }
  else if(auto* Arch = dyn_cast<object::Archive>(BinOrErr->get()))
    Result = handleArchive(env, Filename, *Arch, HandleObj, map);
  return Result;
}

static bool handleFile(Environment* env, StringRef Filename, HandlerFn HandleObj, SourceMap* map)
{
  map->file = innative::utility::AllocString(*env, Filename.str());

  ErrorOr<std::unique_ptr<MemoryBuffer>> BuffOrErr = MemoryBuffer::getFileOrSTDIN(Filename);
  if(!error(env, Filename, BuffOrErr.getError()))
    return false;
  std::unique_ptr<MemoryBuffer> Buffer = std::move(BuffOrErr.get());
  return handleBuffer(env, Filename, *Buffer, HandleObj, map);
}

template<class T> void resizeSourceMap(Environment* env, T*& root, size_t& size, size_t n)
{
  if(n <= size)
    return;
  T* p = innative::utility::tmalloc<T>(*env, n);
  if(p && root && size)
    innative::utility::tmemcpy<T>(p, n, root, size);
  if(p)
    memset(p + size, 0, sizeof(T) * (n - size));

  size = n;
  root = p;
}

bool DumpSourceMap(Environment* env, DWARFContext& DICtx, SourceMap* map, size_t code_section_offset)
{
  size_t file_offset;
  size_t content_offset;
  size_t mapping_offset;

  resizeSourceMap(env, map->mappings, map->n_mappings, 1); // For binary files, there is only ever 1 line
  if(!map->mappings)
    return false;

  for(auto& CU : DICtx.compile_units())
  {
    file_offset    = map->n_sources;
    content_offset = map->n_sourcesContent;
    mapping_offset = map->mappings[0].n_segments;

    auto linetable  = DICtx.getLineTableForUnit(CU.get());
    auto& filenames = linetable->Prologue.FileNames;

    resizeSourceMap(env, map->sources, map->n_sources, map->n_sources + filenames.size());
    resizeSourceMap(env, map->sourcesContent, map->n_sourcesContent, map->n_sourcesContent + filenames.size());
    if(!map->sources || !map->sourcesContent)
      return false;

    for(size_t i = 0; i < filenames.size(); ++i)
    {
      map->sources[file_offset + i] = filenames[i].Name.getAsCString().hasValue() ?
                                        innative::utility::AllocString(*env, filenames[i].Name.getAsCString().getValue()) :
                                        "";
      map->sourcesContent[content_offset + i] =
        filenames[i].Source.getAsCString().hasValue() ?
          innative::utility::AllocString(*env, filenames[i].Source.getAsCString().getValue()) :
          "";
    }

    resizeSourceMap(env, map->mappings[0].segments, map->mappings[0].n_segments, mapping_offset + linetable->Rows.size());
    if(!map->mappings[0].segments)
      return false;

    for(auto& row : linetable->Rows)
    {
      if(!row.Line)
        continue;
      map->mappings[0].segments[mapping_offset].column          = row.Address.Address + code_section_offset;
      map->mappings[0].segments[mapping_offset].original_column = row.Column;
      map->mappings[0].segments[mapping_offset].original_line   = row.Line;
      map->mappings[0].segments[mapping_offset].source_index    = row.File - 1 + file_offset;

      ++mapping_offset;
      assert(mapping_offset <= map->mappings[0].n_segments);
    }

    map->mappings[0].n_segments = mapping_offset;
  }

  std::sort(map->mappings[0].segments, map->mappings[0].segments + map->mappings[0].n_segments,
            [](SourceMapSegment& a, SourceMapSegment& b) { return a.column < b.column; });

  map->x_google_linecount = map->mappings[0].n_segments;
  return true;
}

enum IN_ERROR DWARFSourceMap(struct IN_WASM_ENVIRONMENT* env, SourceMap* map, const char* obj, size_t len)
{
  bool success = true;
  map->version = 3;
  if(!len)
    success = handleFile(env, obj, DumpSourceMap, map);
  else
    success = handleBuffer(env, "memorybuf", MemoryBufferRef(llvm::StringRef(obj, len), "memorybuf"), DumpSourceMap, map);
  return success ? ERR_SUCCESS : ERR_FATAL_FILE_ERROR;
}