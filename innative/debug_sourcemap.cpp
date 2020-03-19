// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "debug_sourcemap.h"
#include "compile.h"
#include "utility.h"
#include <algorithm>

using namespace innative;
using namespace utility;
using namespace llvm::dwarf;

DebugSourceMap::DebugSourceMap(SourceMap* s, Compiler* compiler, llvm::Module& m, const char* name, const char* filepath,
                               char target) :
  sourcemap(s), Debugger(compiler, m, name, filepath, target), curscopeindex(0), cursegment(0)
{
  // If we have a sourcemap, check to see if the files exist. If they don't, see if we can reconstruct them
  path root      = GetPath(sourcemap->sourceRoot);
  path workdir   = GetWorkingDir();
  path parentdir = GetPath(filepath).parent_path();

  auto check = [](const path& file) -> bool {
    FILE* f = 0;
    FOPEN(f, file.c_str(), "rb");
    if(f)
    {
      fclose(f);
      return true;
    }
    return false;
  };

  for(size_t i = 0; i < sourcemap->n_sources; ++i)
  {
    path source = GetPath(sourcemap->sources[i]);
    if(check(root / source))
      sourcemap->sources[i] = AllocString(_compiler->env, (root / source).u8string());
    else if(check(source))
      sourcemap->sources[i] = AllocString(_compiler->env, GetAbsolutePath(source).u8string());
    else if(check(workdir / source))
      sourcemap->sources[i] = AllocString(_compiler->env, (workdir / source).u8string());
    else if(check(parentdir / source))
      sourcemap->sources[i] = AllocString(_compiler->env, (parentdir / source).u8string());
    else if(i < sourcemap->n_sourcesContent && sourcemap->sourcesContent[i] != 0 && sourcemap->sourcesContent[i][0] != 0)
    {
      source = temp_directory_path() / source.filename();
      if(DumpFile(source, sourcemap->sourcesContent[i], strlen(sourcemap->sourcesContent[i])))
        sourcemap->sources[i] = AllocString(_compiler->env, source.u8string());
    }
  }

  for(size_t i = 0; i < sourcemap->n_sources; ++i)
  {
    path p = GetPath(sourcemap->sources[i]);
    files.push_back(_dbuilder->createFile(p.filename().u8string(), p.parent_path().u8string()));
  }

  if(s->n_innative_types)
    types.resize(s->n_innative_types);
}

llvm::DIFile* DebugSourceMap::GetSourceFile(size_t i) { return (i < files.size()) ? files[i] : dunit; }

SourceMapFunction* DebugSourceMap::GetSourceFunction(unsigned int offset)
{
  auto end             = sourcemap->x_innative_functions + sourcemap->n_innative_functions;
  SourceMapFunction* i = std::lower_bound(sourcemap->x_innative_functions, end, offset,
                                          [](const SourceMapFunction& f, unsigned int o) { return f.range.low < o; });
  return (i >= end /*|| i->range.low != offset*/) ? nullptr : i;
}

void DebugSourceMap::FuncDecl(llvm::Function* fn, unsigned int offset, unsigned int line, bool optimized)
{
  // Use function low_PC to find the debug entry
  SourceMapFunction* f = GetSourceFunction(offset);

  if(!f)
  {
    FunctionDebugInfo(fn, fn->getName(), optimized, true, false, dunit, line, 0);
    return;
  }

  assert(f->range.scope < sourcemap->n_innative_scopes);
  if(f->range.scope >= sourcemap->n_innative_scopes)
    return;
  auto& scope = sourcemap->x_innative_scopes[f->range.scope];
  auto name   = (scope.name_index < sourcemap->n_names) ? sourcemap->names[scope.name_index] : fn->getName();

  llvm::SmallVector<llvm::Metadata*, 8> dwarfTys = { GetDebugType(f->type_index) };
  for(unsigned int i = 0; i < scope.n_variables; ++i)
  {
    auto& v = sourcemap->x_innative_variables[scope.variables[i]];
    if(v.tag == DW_TAG_formal_parameter)
      dwarfTys.push_back(GetDebugType(v.type_index));
    else if(v.tag == DW_TAG_unspecified_parameters)
      dwarfTys.push_back(_dbuilder->createUnspecifiedParameter());
  }
  auto subtype =
    _dbuilder->createSubroutineType(_dbuilder->getOrCreateTypeArray(dwarfTys), llvm::DINode::FlagZero,
                                    (fn->getCallingConv() == llvm::CallingConv::C) ? DW_CC_normal : DW_CC_nocall);

  FunctionDebugInfo(fn, name, optimized, true, false, GetSourceFile(f->source_index), f->original_line, 0, subtype);
}

void DebugSourceMap::FuncBody(llvm::Function* fn, size_t indice, FunctionDesc& desc, FunctionBody& body)
{
  subprograms.resize(files.size());
  for(auto& i : subprograms)
    i = nullptr;

  curscopeindex = 0;
  if(sourcemap->n_innative_ranges > 0)
  {
    auto end   = sourcemap->x_innative_ranges + sourcemap->n_innative_ranges;
    auto range = std::lower_bound(sourcemap->x_innative_ranges, end, body.column,
                                  [](const SourceMapRange& s, unsigned int i) { return s.low < i; });
    if(range != end && range->scope < sourcemap->n_innative_scopes)
      curscopeindex = range->scope;
  }

  cursegment = 0;
  if(sourcemap->n_innative_ranges > 0)
  {
    auto end     = sourcemap->segments + sourcemap->n_segments;
    auto segment = std::lower_bound(sourcemap->segments, end, ((body.line - 1ULL) << 32) | body.column,
                                    [](const SourceMapSegment& s, uint64_t i) { return s.linecolumn < i; });
    cursegment   = segment - sourcemap->segments;
  }

  _compiler->builder.SetCurrentDebugLocation(
    llvm::DILocation::get(_compiler->ctx, fn->getSubprogram()->getLine(), 0, fn->getSubprogram()));

  // Unfortunately we can't actually gaurantee all the scopes will pop due to imprecise debug information
  while(scopes.Size() > 0)
    scopes.Pop();

  scopecache.resize(0); // Wipe the cache
  scopecache.resize(sourcemap->n_innative_scopes);
  _curscope = fn->getSubprogram();
  _curbody  = &body;
}

void DebugSourceMap::PostFuncBody(llvm::Function* fn, FunctionBody& body) {}

void DebugSourceMap::DebugSetGlobal(int index) {}

void DebugSourceMap::FuncParam(llvm::Function* fn, size_t index, FunctionDesc& desc) {}

void DebugSourceMap::FuncLocal(llvm::Function* fn, size_t indice, FunctionDesc& desc) {}

void DebugSourceMap::UpdateLocation(Instruction& i)
{
  llvm::DILocation* loc = 0;

  while(cursegment < sourcemap->n_segments &&
        sourcemap->segments[cursegment].linecolumn < ((i.line - 1ULL) << 32 | i.column))
  {
    if(loc) // If we have a pending location, we need to create a nop instruction to hold it
      _compiler->builder.CreateIntrinsic(llvm::Intrinsic::donothing, {}, {})->setDebugLoc(loc);

    auto& s = sourcemap->segments[cursegment++];

    if(files[s.source_index] == _curscope->getFile() || s.source_index >= subprograms.size())
      loc = llvm::DILocation::get(_compiler->ctx, s.original_line, s.original_column, _curscope);
    else
    {
      if(!subprograms[s.source_index])
      {
        auto original = _curscope->getSubprogram();
        subprograms[s.source_index] =
          _dbuilder->createFunction(original->getScope(), original->getName(), original->getLinkageName(),
                                    files[s.source_index], original->getLine(), original->getType(),
                                    original->getScopeLine(), original->getFlags(), original->getSPFlags());
      }

      loc = _compiler->builder.getCurrentDebugLocation();
      if(loc->getInlinedAt())
        loc = loc->getInlinedAt();
      loc = llvm::DILocation::get(_compiler->ctx, s.original_line, s.original_column, subprograms[s.source_index], loc);
    }
  }

  if(loc)
    _compiler->builder.SetCurrentDebugLocation(loc);
}

void DebugSourceMap::UpdateVariables(llvm::Function* fn, SourceMapScope& scope) {}

void DebugSourceMap::DebugIns(llvm::Function* fn, Instruction& i)
{
  // Pop scopes. Parent scopes should always contain child scopes, if this isn't true someone screwed up
  while(scopes.Size() > 0 && i.column > sourcemap->x_innative_ranges[scopes.Peek()].high)
    scopes.Pop();
  _curscope = scopes.Size() > 0 ? scopecache[sourcemap->x_innative_ranges[scopes.Peek()].scope] : fn->getSubprogram();
  assert(_curscope);

  // push scopes and any variables they contain
  while(curscopeindex < sourcemap->n_innative_ranges && i.column >= sourcemap->x_innative_ranges[curscopeindex].low)
  {
    auto& cur = sourcemap->x_innative_ranges[curscopeindex];
    if(i.column <= cur.high && cur.scope < scopecache.size())
    {
      auto& scope = sourcemap->x_innative_scopes[cur.scope];
      scopes.Push(curscopeindex);
      if(!scopecache[cur.scope])
      {
        llvm::DILocation* l   = _compiler->builder.getCurrentDebugLocation();
        auto file             = GetSourceFile(sourcemap->segments[cursegment].source_index);
        scopecache[cur.scope] = _dbuilder->createLexicalBlock(_curscope, file, l->getLine(), l->getColumn());
      }
      _curscope = scopecache[cur.scope];
      assert(cur.scope < sourcemap->n_innative_scopes);

      UpdateVariables(fn, scope);
    }

    ++curscopeindex;
  }

  UpdateLocation(i);
}

void DebugSourceMap::DebugGlobal(llvm::GlobalVariable* v, llvm::StringRef name, size_t line)
{
  v->addDebugInfo(_dbuilder->createGlobalVariableExpression(dcu, name, v->getName(), dunit, static_cast<unsigned int>(line),
                                                            CreateDebugType(v->getType()->getElementType()),
                                                            !v->hasValidDeclarationLinkage(),
                                                            _dbuilder->createExpression()));
}

llvm::DINode::DIFlags DebugSourceMap::GetFlags(unsigned short flags)
{
  llvm::DINode::DIFlags diflags = llvm::DINode::FlagZero;
  if((flags & IN_SOURCE_TYPE_PROTECTED) == IN_SOURCE_TYPE_PROTECTED)
    diflags |= llvm::DINode::FlagProtected;
  else if(flags & IN_SOURCE_TYPE_PUBLIC)
    diflags |= llvm::DINode::FlagPublic;
  else if(flags & IN_SOURCE_TYPE_PRIVATE)
    diflags |= llvm::DINode::FlagPrivate;
  if(flags & IN_SOURCE_TYPE_VIRTUAL || flags & IN_SOURCE_TYPE_PURE_VIRTUAL)
    diflags |= llvm::DINode::FlagVirtual;
  if(flags & IN_SOURCE_TYPE_ARTIFICIAL)
    diflags |= llvm::DINode::FlagArtificial;
  return diflags;
}

llvm::DIType* DebugSourceMap::GetDebugType(size_t index, llvm::DIType* parent)
{
  if(index == (size_t)~0)
    return diVoid;
  auto& type = sourcemap->x_innative_types[index];

  if(index < types.size() && !types[index])
  {
    const char* name   = type.name_index < sourcemap->n_names ? sourcemap->names[type.name_index] : 0;
    llvm::DIFile* file = GetSourceFile(type.source_index);

    char namebuf[20] = { 0 };
    if(!name)
    {
      SPRINTF(namebuf, 20, "t#%zu", index);
      name = namebuf;
    }

    switch(type.tag)
    {
    case DW_TAG_base_type: types[index] = _dbuilder->createBasicType(name, type.bit_size, type.encoding); break;
    case DW_TAG_array_type:
      if(auto ty = GetDebugType(type.type_index))
      {
        llvm::Metadata* range[] = { _dbuilder->getOrCreateSubrange(0, type.n_types) };
        types[index] = _dbuilder->createArrayType(!type.bit_size ? ty->getSizeInBits() * type.n_types : type.bit_size,
                                                  !type.byte_align ? ty->getAlignInBits() : (type.byte_align << 3), ty,
                                                  !type.n_types ? llvm::DINodeArray() : _dbuilder->getOrCreateArray(range));
      }
      break;
    case DW_TAG_const_type:
    case DW_TAG_volatile_type:
    case DW_TAG_restrict_type:
    case DW_TAG_atomic_type:
      if(auto ty = GetDebugType(type.type_index))
        types[index] = _dbuilder->createQualifiedType(type.tag, ty);
      break;
    case DW_TAG_pointer_type:
      if(auto ty = GetDebugType(type.type_index))
        types[index] = _dbuilder->createPointerType(ty, 32, 0, llvm::None, name);
      break;
    case DW_TAG_rvalue_reference_type:
    case DW_TAG_reference_type:
      if(auto ty = GetDebugType(type.type_index))
        types[index] = _dbuilder->createReferenceType(type.tag, ty, type.bit_size, type.byte_align << 3, llvm::None);
      break;
    case DW_TAG_typedef:
      if(auto ty = GetDebugType(type.type_index))
        types[index] = _dbuilder->createTypedef(ty, name, file, type.original_line, file);
      break;
    case DW_TAG_ptr_to_member_type:
      if(type.n_types == 2)
      {
        auto base = GetDebugType(type.types[1]);
        auto cls  = GetDebugType(type.types[0]);
        if(base && cls)
          types[index] = _dbuilder->createMemberPointerType(base, cls, type.bit_size, type.byte_align << 3);
      }
      break;
    case DW_TAG_class_type:
    case DW_TAG_structure_type:
    {
      llvm::DICompositeType* composite;
      if(type.tag == DW_TAG_class_type)
        composite = _dbuilder->createClassType(file, name, file, type.original_line, type.bit_size, type.byte_align << 3,
                                               type.bit_offset, llvm::DINode::FlagZero, nullptr, llvm::DINodeArray());
      else
        composite = _dbuilder->createStructType(file, name, file, type.original_line, type.bit_size, type.byte_align << 3,
                                                llvm::DINode::FlagZero, nullptr, llvm::DINodeArray());
      types[index] = composite;

      llvm::SmallVector<llvm::Metadata*, 8> elements;
      for(size_t i = 0; i < type.n_types; ++i)
        if(auto ty = GetDebugType(type.types[i], composite))
          elements.push_back(ty);

      _dbuilder->replaceArrays(composite, _dbuilder->getOrCreateArray(elements));
      break;
    }
    case DW_TAG_union_type:
    {
      auto composite = _dbuilder->createUnionType(file, name, file, type.original_line, type.bit_size, type.byte_align << 3,
                                                  GetFlags(type.flags), llvm::DINodeArray());
      types[index]   = composite;

      llvm::SmallVector<llvm::Metadata*, 8> elements;
      for(size_t i = 0; i < type.n_types; ++i)
        if(auto ty = GetDebugType(type.types[i]))
          elements.push_back(ty);

      _dbuilder->replaceArrays(composite, _dbuilder->getOrCreateArray(elements));
      break;
    }
    case DW_TAG_inheritance:
      if(!parent)
        break;
      if(auto ty = GetDebugType(type.type_index))
      {
        auto offset = type.bit_offset;
        if(type.flags & IN_SOURCE_TYPE_VIRTUAL)
          offset /= 8; // it's in bytes if it's virtual for some insane reason

        types[index] = _dbuilder->createInheritance(parent, ty, offset, 0, GetFlags(type.flags));
      }
      break;
    case DW_TAG_friend:
      if(!parent)
        break;
      if(auto ty = GetDebugType(type.type_index))
        types[index] = _dbuilder->createFriend(parent, ty);
      break;
    case DW_TAG_member:
      if(auto ty = GetDebugType(type.type_index))
        types[index] = _dbuilder->createMemberType(file, name, file, type.original_line, type.bit_size,
                                                   type.byte_align << 3, type.bit_offset, GetFlags(type.flags), ty);
      break;
    case DW_TAG_enumeration_type:
    {
      auto base = GetDebugType(type.type_index);
      if(!base)
        base = diI32;
      llvm::SmallVector<llvm::Metadata*, 8> elements;
      for(size_t i = 0; i < type.n_types; ++i)
      {
        if(type.types[i] < sourcemap->n_innative_enumerators)
        {
          auto& e = sourcemap->x_innative_enumerators[type.types[i]];
          auto id = e.name_index >= sourcemap->n_names ? "" : sourcemap->names[e.name_index];
          elements.push_back(_dbuilder->createEnumerator(id, e.val, false)); // TODO: check if base is unsigned
        }
      }
      types[index] = _dbuilder->createEnumerationType(file, name, file, type.original_line, type.bit_size,
                                                      type.byte_align << 3, _dbuilder->getOrCreateArray(elements), base, "",
                                                      (type.flags & IN_SOURCE_TYPE_ENUM_CLASS) != 0);
    }
    break;
    case DW_TAG_subroutine_type:
    {
      llvm::SmallVector<llvm::Metadata*, 8> params;
      for(size_t i = 0; i < type.n_types; ++i)
        params.push_back(GetDebugType(type.types[i]));

      types[index] = _dbuilder->createSubroutineType(_dbuilder->getOrCreateTypeArray(params), GetFlags(type.flags));
      break;
    }
    }
  }

  return index < types.size() ? types[index] : nullptr;
}
void DebugSourceMap::PushBlock(llvm::DILocalScope* scope, const llvm::DebugLoc& loc) {}
