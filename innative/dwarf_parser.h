// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__DWARF_PARSER_H
#define IN__DWARF_PARSER_H

#include "innative/schema.h"
#include "innative/sourcemap.h"
#include "llvm.h"
#include "utility.h"

KHASH_DECLARE(mapname, const char*, size_t);
KHASH_DECLARE(maptype, SourceMapType*, size_t);

namespace innative {
  static kh_inline khint_t kh_type_hash(const SourceMapType* s)
  {
#ifdef IN_64BIT // if constexpr still generates a false warning about size_t
    return kh_int64_hash_func(s->offset);
#else
    return kh_int_hash_func(s->offset);
#endif
  }
  static kh_inline khint_t kh_type_equal(const SourceMapType* a, const SourceMapType* b) { return a->offset == b->offset; }

  class DWARFParser
  {
  public:
    typedef bool (DWARFParser::*HandlerFn)(llvm::DWARFContext& DICtx, size_t code_section_offset);

    DWARFParser(struct IN_WASM_ENVIRONMENT* env, SourceMap* map);
    ~DWARFParser();

    bool error(llvm::StringRef Prefix, std::error_code EC);
    bool handleBuffer(llvm::StringRef Filename, llvm::MemoryBufferRef Buffer, HandlerFn HandleObj);
    bool handleArchive(llvm::StringRef Filename, llvm::object::Archive& Arch, HandlerFn HandleObj);
    bool handleFile(llvm::StringRef Filename, HandlerFn HandleObj);
    llvm::DWARFDie AsReferencedDIE(const llvm::DWARFFormValue& V, const llvm::DWARFUnit& unit);
    size_t GetSourceMapName(const char* name);
    const char* GetDieName(const llvm::DWARFDie& die);
    bool DumpSourceMap(llvm::DWARFContext& DICtx, size_t code_section_offset);
    void ResolveDWARFBitSize(const llvm::DWARFDie& die, SourceMapType* ptype);
    void ResolveDWARFTypeFlags(const llvm::DWARFDie& die, SourceMapType* ptype);

    bool ParseDWARFChild(llvm::DWARFContext& DICtx, SourceMapScope* parent, const llvm::DWARFDie& die, llvm::DWARFUnit* CU,
                         size_t code_section_offset);
    enum IN_ERROR ParseDWARF(const char* obj, size_t len);

    template<llvm::dwarf::Attribute Attr1, llvm::dwarf::Attribute Attr2, typename T>
    void ResolveBitAttribute(const llvm::DWARFDie& die, T& target)
    {
      if(auto attr = die.find(Attr1))
        target = (T)attr->getAsUnsignedConstant().getValueOr(0);
      if(auto attr = die.find(Attr2))
        target = (T)attr->getAsUnsignedConstant().getValueOr(0) << 3;
    }

    template<class T> void resizeSourceMap(T*& root, size_t& size, size_t n)
    {
      if(n <= size)
        return;
      T* p = innative::utility::template tmalloc<T>(*env, n);
      if(p && root && size)
        innative::utility::template tmemcpy<T>(p, n, root, size);
      if(p)
        memset(p + size, 0, sizeof(T) * (n - size));

      size = n;
      root = p;
    }

  protected:
    size_t GetSourceMapType(llvm::DWARFUnit& unit, const llvm::DWARFDie& die);
    size_t GetSourceMapTypeRef(llvm::DWARFUnit& unit, const llvm::DWARFFormValue& type);
    bool HasIsStmt(size_t i, const llvm::DWARFDebugLine::LineTable::RowVector& rows);

    Environment* env;
    SourceMap* map;
    kh_maptype_t* maptype;
    size_t n_types;
    kh_mapname_t* mapname;
    size_t n_names;
    size_t n_variables;
    size_t n_ranges;
    size_t n_scopes;
    size_t n_functions;
    size_t file_offset;
    size_t content_offset;
    size_t mapping_offset;
  };
}

#endif