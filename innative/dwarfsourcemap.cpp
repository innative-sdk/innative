// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "innative/sourcemap.h"
#include "innative/schema.h"
#include "util.h"
#include "stream.h"

using llvm::DWARFContext;
using llvm::DWARFDebugLoc;
using llvm::DWARFDie;
using llvm::DWARFFormValue;
using llvm::dyn_cast;
using llvm::Error;
using llvm::MemoryBuffer;
using llvm::MemoryBufferRef;
using llvm::Optional;
using llvm::StringRef;
using namespace llvm::object;
using namespace llvm::dwarf;

typedef bool (*HandlerFn)(Environment* env, DWARFContext& DICtx, SourceMap* map, size_t code_section_offset);

static kh_inline khint_t kh_type_hash(const SourceMapType& s)
{
#ifdef IN_64BIT // if constexpr still generates a false warning about size_t
  return kh_int64_hash_func(s.offset);
#else
  return kh_int_hash_func(s.offset);
#endif
}
static kh_inline khint_t kh_type_equal(const SourceMapType& a, const SourceMapType& b) { return a.offset == b.offset; }

KHASH_INIT(mapname, const char*, size_t, 1, kh_str_hash_func, kh_str_hash_equal)
KHASH_INIT(maptype, SourceMapType, size_t, 1, kh_type_hash, kh_type_equal)

static bool error(const Environment* env, llvm::StringRef Prefix, std::error_code EC)
{
  if(!EC)
    return true;
  fwrite(Prefix.begin(), 1, Prefix.size(), env->log);
  return false;
}

static bool handleBuffer(Environment* env, StringRef Filename, MemoryBufferRef Buffer, HandlerFn HandleObj, SourceMap* map);

static bool handleArchive(Environment* env, StringRef Filename, Archive& Arch, HandlerFn HandleObj, SourceMap* map)
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
          s.ReadVarUInt32(err); // read past payload integer
          code_section_offset = s.pos - 1;
          break;
        }
        s.pos += s.ReadVarUInt32(err);

        if(err < 0)
          break;
      }
    }
  }

  llvm::Expected<std::unique_ptr<Binary>> BinOrErr = createBinary(Buffer);
  if(!error(env, Filename, errorToErrorCode(BinOrErr.takeError())))
    return false;

  bool Result = true;
  if(auto* Obj = dyn_cast<ObjectFile>(BinOrErr->get()))
  {
    std::unique_ptr<DWARFContext> DICtx = DWARFContext::create(*Obj);
    Result                              = HandleObj(env, *DICtx, map, code_section_offset);
  }
  else if(auto* Fat = dyn_cast<MachOUniversalBinary>(BinOrErr->get()))
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
  else if(auto* Arch = dyn_cast<Archive>(BinOrErr->get()))
    Result = handleArchive(env, Filename, *Arch, HandleObj, map);
  return Result;
}

static bool handleFile(Environment* env, StringRef Filename, HandlerFn HandleObj, SourceMap* map)
{
  map->file = innative::utility::AllocString(*env, Filename.str());

  llvm::ErrorOr<std::unique_ptr<MemoryBuffer>> BuffOrErr = MemoryBuffer::getFileOrSTDIN(Filename);
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

DWARFDie AsReferencedDIE(const DWARFFormValue& V, const llvm::DWARFUnit& unit)
{
  if(auto SpecRef = V.getAsRelativeReference())
  {
    if(SpecRef->Unit)
      return SpecRef->Unit->getDIEForOffset(SpecRef->Unit->getOffset() + SpecRef->Offset);
    if(auto SpecUnit = unit.getUnitVector().getUnitForOffset(SpecRef->Offset))
      return SpecUnit->getDIEForOffset(SpecRef->Offset);
  }
  return DWARFDie();
}

size_t GetSourceMapName(Environment* env, kh_mapname_t* h, const char* name, size_t& index, SourceMap* map)
{
  auto iter = kh_get_mapname(h, name);
  if(iter >= kh_end(h))
  {
    int r;
    iter = kh_put_mapname(h, innative::utility::AllocString(*env, name), &r);
    if(r < 0)
      return (size_t)~0;
    kh_value(h, iter) = index++;

    if(index > map->n_names)
      resizeSourceMap(env, map->names, map->n_names, index * 2);
    map->names[index - 1] = kh_key(h, iter);
  }
  return kh_value(h, iter);
};

template<llvm::dwarf::Attribute Attr1, llvm::dwarf::Attribute Attr2, typename T>
void ResolveBitAttribute(const DWARFDie& die, T& target)
{
  if(auto attr = die.find(Attr1))
    target = attr->getAsUnsignedConstant().getValueOr(0);
  if(auto attr = die.find(Attr2))
    target = attr->getAsUnsignedConstant().getValueOr(0) << 3;
}

void ResolveDWARFBitSize(const DWARFDie& die, kh_maptype_t* maptype, khint_t iter)
{
  return ResolveBitAttribute<DW_AT_bit_size, DW_AT_byte_size>(die, kh_key(maptype, iter).bit_size);
}

void ResolveDWARFTypeFlags(const DWARFDie& die, kh_maptype_t* maptype, khint_t iter)
{
  if(auto flag = die.find(DW_AT_accessibility))
  {
    switch(flag->getAsUnsignedConstant().getValueOr(0))
    {
    case DW_ACCESS_public: kh_key(maptype, iter).flags |= IN_SOURCE_TYPE_PUBLIC; break;
    case DW_ACCESS_private: kh_key(maptype, iter).flags |= IN_SOURCE_TYPE_PRIVATE; break;
    case DW_ACCESS_protected: kh_key(maptype, iter).flags |= IN_SOURCE_TYPE_PROTECTED; break;
    }
  }
  if(auto flag = die.find(DW_AT_virtuality))
  {
    switch(flag->getAsUnsignedConstant().getValueOr(0))
    {
    case DW_VIRTUALITY_virtual: kh_key(maptype, iter).flags |= IN_SOURCE_TYPE_VIRTUAL; break;
    case DW_VIRTUALITY_pure_virtual: kh_key(maptype, iter).flags |= IN_SOURCE_TYPE_PURE_VIRTUAL; break;
    }
  }
  if(auto flag = die.find(DW_AT_friend))
    kh_key(maptype, iter).flags |= IN_SOURCE_TYPE_FRIEND;
  if(auto flag = die.find(DW_AT_artificial))
    kh_key(maptype, iter).flags |= IN_SOURCE_TYPE_ARTIFICIAL;
  if(auto flag = die.find(DW_AT_declaration))
    kh_key(maptype, iter).flags |= IN_SOURCE_TYPE_DECLARATION;
  if(auto flag = die.find(DW_AT_default_value))
    kh_key(maptype, iter).flags |= IN_SOURCE_TYPE_DEFAULT;
  if(auto flag = die.find(DW_AT_export_symbols))
    kh_key(maptype, iter).flags |= IN_SOURCE_TYPE_EXPORT;
  if(auto flag = die.find(DW_AT_mutable))
    kh_key(maptype, iter).flags |= IN_SOURCE_TYPE_MUTABLE;
  if(auto flag = die.find(DW_AT_enum_class))
    kh_key(maptype, iter).flags |= IN_SOURCE_TYPE_ENUM_CLASS;
}

size_t GetSourceMapType(Environment* env, llvm::DWARFUnit& unit, kh_maptype_t* maptype, size_t& n_types,
                        const DWARFDie& die, kh_mapname_t* mapname, size_t& n_names, SourceMap* map);

size_t GetSourceMapTypeRef(Environment* env, llvm::DWARFUnit& unit, kh_maptype_t* maptype, size_t& n_types,
                           const DWARFFormValue& type, kh_mapname_t* mapname, size_t& n_names, SourceMap* map)
{
  if(DWARFDie die = AsReferencedDIE(type, unit))
    return GetSourceMapType(env, unit, maptype, n_types, die, mapname, n_names, map);
  return (size_t)~0;
}

const char* GetDieName(const DWARFDie& die, kh_maptype_t* maptype, size_t n_names, SourceMap* map)
{
  if(auto iter = kh_get_maptype(maptype, SourceMapType{ die.getOffset() }); kh_exist2(maptype, iter))
    if(kh_key(maptype, iter).name_index < n_names)
      return map->names[kh_key(maptype, iter).name_index];
  return 0;
}

size_t GetSourceMapType(Environment* env, llvm::DWARFUnit& unit, kh_maptype_t* maptype, size_t& n_types,
                        const DWARFDie& die, kh_mapname_t* mapname, size_t& n_names, SourceMap* map)
{
  switch(die.getTag())
  {
  case DW_TAG_array_type:
  case DW_TAG_class_type:
  case DW_TAG_interface_type:
  case DW_TAG_enumeration_type:
  case DW_TAG_pointer_type:
  case DW_TAG_reference_type:
  case DW_TAG_rvalue_reference_type:
  case DW_TAG_string_type:
  case DW_TAG_structure_type:
  case DW_TAG_subroutine_type:
  case DW_TAG_union_type:
  case DW_TAG_ptr_to_member_type:
  case DW_TAG_set_type:
  case DW_TAG_subrange_type:
  case DW_TAG_base_type:
  case DW_TAG_const_type:
  case DW_TAG_file_type:
  case DW_TAG_packed_type:
  case DW_TAG_volatile_type:
  case DW_TAG_template_value_parameter:
  case DW_TAG_template_type_parameter:
  case DW_TAG_formal_parameter:
  case DW_TAG_member:
  case DW_TAG_inheritance:
  case DW_TAG_typedef: break;
  default: return (size_t)~0;
  }

  int r;
  auto iter = kh_put_maptype(maptype, SourceMapType{ die.getOffset() }, &r);
  if(r > 0)
  {
    kh_value(maptype, iter)            = n_types++;
    kh_key(maptype, iter).tag          = die.getTag();
    kh_key(maptype, iter).bit_size     = 0;
    kh_key(maptype, iter).byte_align   = 0;
    kh_key(maptype, iter).name_index   = (size_t)~0;
    kh_key(maptype, iter).source_index = (size_t)~0;
    kh_key(maptype, iter).type_index   = (size_t)~0;

    if(auto file = die.find(DW_AT_decl_file))
    {
      if(const auto* LT = unit.getContext().getLineTableForUnit(&unit))
        kh_key(maptype, iter).source_index = file->getAsUnsignedConstant().getValueOr(0) - 1;
    }
    if(auto line = die.find(DW_AT_decl_line))
      kh_key(maptype, iter).original_line = line->getAsUnsignedConstant().getValueOr(0);
    if(auto name = die.find(DW_AT_name))
    {
      if(auto attr = name->getAsCString())
        kh_key(maptype, iter).name_index = GetSourceMapName(env, mapname, attr.getValue(), n_names, map);
    }
    if(auto align = die.find(DW_AT_alignment))
      kh_key(maptype, iter).byte_align = (unsigned short)align->getAsUnsignedConstant().getValueOr(0);
    ResolveDWARFTypeFlags(die, maptype, iter);

    // Figure out what kind of type this is
    switch(die.getTag())
    {
    case DW_TAG_base_type:
      kh_key(maptype, iter).n_types  = 0;
      kh_key(maptype, iter).encoding = 0;
      ResolveDWARFBitSize(die, maptype, iter);

      if(auto line = die.find(DW_AT_encoding))
        kh_key(maptype, iter).encoding = (unsigned short)line->getAsUnsignedConstant().getValueOr(0);

      break;
    case DW_TAG_array_type:
      ResolveBitAttribute<DW_AT_bit_stride, DW_AT_byte_stride>(die, kh_key(maptype, iter).bit_stride);
      ResolveDWARFBitSize(die, maptype, iter);
    case DW_TAG_atomic_type:
    case DW_TAG_const_type:
    case DW_TAG_immutable_type:
    case DW_TAG_packed_type:
    case DW_TAG_pointer_type:
    case DW_TAG_reference_type:
    case DW_TAG_restrict_type:
    case DW_TAG_rvalue_reference_type:
    case DW_TAG_shared_type:
    case DW_TAG_volatile_type:
    case DW_TAG_ptr_to_member_type:
    case DW_TAG_typedef:
    {
      kh_key(maptype, iter).n_types = 1;
      const char* basename          = "void";

      if(auto innertype = die.find(DW_AT_type))
      {
        if(DWARFDie innerdie = AsReferencedDIE(innertype.getValue(), unit))
        {
          kh_key(maptype, iter).type_index = GetSourceMapType(env, unit, maptype, n_types, innerdie, mapname, n_names, map);
          iter                             = kh_get_maptype(maptype, SourceMapType{ die.getOffset() });
          if(auto diename = GetDieName(innerdie, maptype, n_names, map); diename != nullptr)
            basename = diename;
        }
      } // If this is false, it's a void type, like const void*

      if(kh_key(maptype, iter).name_index == (size_t)~0)
      {
        std::string modifier = basename;
        switch(die.getTag())
        {
        case DW_TAG_atomic_type: modifier = "atomic " + modifier; break;
        case DW_TAG_const_type: modifier = "const " + modifier; break;
        case DW_TAG_immutable_type: modifier = "immutable " + modifier; break;
        case DW_TAG_packed_type: modifier = "packed " + modifier; break;
        case DW_TAG_pointer_type: modifier = modifier + "*"; break;
        case DW_TAG_reference_type: modifier = modifier + "&"; break;
        case DW_TAG_restrict_type: modifier = "restrict " + modifier; break;
        case DW_TAG_rvalue_reference_type: modifier = modifier + "&&"; break;
        case DW_TAG_shared_type: modifier = modifier + "shared "; break;
        case DW_TAG_volatile_type: modifier = "volatile " + modifier; break;
        case DW_TAG_ptr_to_member_type: modifier = "(*" + modifier + ")"; break;
        }
        kh_key(maptype, iter).name_index = GetSourceMapName(env, mapname, modifier.c_str(), n_names, map);
      }

      break;
    }
    case DW_TAG_enumeration_type:
    {
      ResolveDWARFBitSize(die, maptype, iter);
      if(auto enumtype = die.find(DW_AT_type))
      {
        kh_key(maptype, iter).type_index =
          GetSourceMapTypeRef(env, unit, maptype, n_types, enumtype.getValue(), mapname, n_names, map);
        iter = kh_get_maptype(maptype, SourceMapType{ die.getOffset() });
      }

      size_t count = 0;
      for(auto& child : die.children())
        count += child.getTag() == DW_TAG_enumerator;

      kh_key(maptype, iter).enumerators = innative::utility::tmalloc<size_t>(*env, count);
      size_t n_enumerators              = map->n_innative_enumerators;
      resizeSourceMap(env, map->x_innative_enumerators, map->n_innative_enumerators, n_enumerators + count);

      for(auto& child : die.children())
      {
        if(child.getTag() != DW_TAG_enumerator)
          continue;

        auto& e = map->x_innative_enumerators[n_enumerators];
        if(auto value = child.find(DW_AT_const_value))
          if(auto constant = value->getAsUnsignedConstant())
            e.val = constant.getValue();
          else
            e.val = value->getAsSignedConstant().getValueOr(0);

        if(auto name = child.find(DW_AT_name))
        {
          if(auto attr = name->getAsCString())
            e.name_index = GetSourceMapName(env, mapname, attr.getValue(), n_names, map);
        }

        kh_key(maptype, iter).enumerators[kh_key(maptype, iter).n_types++] = n_enumerators++;
      }

      map->n_innative_enumerators = n_enumerators;
      break;
    }
    case DW_TAG_subroutine_type:
    {
      std::string funcptr = "void";

      if(auto innertype = die.find(DW_AT_type))
      {
        if(DWARFDie innerdie = AsReferencedDIE(innertype.getValue(), unit))
        {
          kh_key(maptype, iter).type_index = GetSourceMapType(env, unit, maptype, n_types, innerdie, mapname, n_names, map);
          iter                             = kh_get_maptype(maptype, SourceMapType{ die.getOffset() });
          if(auto diename = GetDieName(innerdie, maptype, n_names, map); diename != nullptr)
            funcptr = diename;
        }
      }
      funcptr += '(';

      size_t count = 0;
      for(auto& child : die.children())
        count++;

      kh_key(maptype, iter).types = innative::utility::tmalloc<size_t>(*env, count);
      if(!kh_key(maptype, iter).types)
        return (size_t)~0;

      bool first = true;
      for(auto& child : die.children())
      {
        if(!first)
          funcptr += ", ";
        auto ty = GetSourceMapType(env, unit, maptype, n_types, child, mapname, n_names, map);
        iter    = kh_get_maptype(maptype, SourceMapType{ die.getOffset() });
        if(ty != (size_t)~0)
          kh_key(maptype, iter).types[kh_key(maptype, iter).n_types++] = ty;

        if(auto innertype = child.find(DW_AT_type))
          if(DWARFDie innerdie = AsReferencedDIE(innertype.getValue(), unit))
            if(auto diename = GetDieName(innerdie, maptype, n_names, map); diename != nullptr)
              funcptr += diename;

        if(auto diename = GetDieName(child, maptype, n_names, map); diename != nullptr)
        {
          funcptr += ' ';
          funcptr += diename;
        }
      }
      funcptr += ')';

      if(kh_key(maptype, iter).name_index == (size_t)~0)
        kh_key(maptype, iter).name_index = GetSourceMapName(env, mapname, funcptr.c_str(), n_names, map);
      break;
    }
    case DW_TAG_class_type:
    case DW_TAG_structure_type:
    case DW_TAG_union_type:
    case DW_TAG_interface_type:
    {
      ResolveDWARFBitSize(die, maptype, iter);

      size_t count = 0;
      for(auto& child : die.children())
        count++;

      kh_key(maptype, iter).types = innative::utility::tmalloc<size_t>(*env, count);
      if(!kh_key(maptype, iter).types)
        return (size_t)~0;

      for(auto& child : die.children())
      {
        auto ty = GetSourceMapType(env, unit, maptype, n_types, child, mapname, n_names, map);
        iter    = kh_get_maptype(maptype, SourceMapType{ die.getOffset() });
        if(ty != (size_t)~0)
          kh_key(maptype, iter).types[kh_key(maptype, iter).n_types++] = ty;
      }
      break;
    }
    case DW_TAG_formal_parameter:
    case DW_TAG_template_value_parameter:
    case DW_TAG_template_type_parameter:
      if(auto innertype = die.find(DW_AT_type))
      {
        kh_key(maptype, iter).n_types = 1;
        kh_key(maptype, iter).type_index =
          GetSourceMapTypeRef(env, unit, maptype, n_types, innertype.getValue(), mapname, n_names, map);
      }
      break;
    case DW_TAG_member:
    case DW_TAG_inheritance:
      ResolveDWARFBitSize(die, maptype, iter);
      ResolveBitAttribute<DW_AT_bit_offset, DW_AT_data_member_location>(die, kh_key(maptype, iter).bit_offset);
      if(auto innertype = die.find(DW_AT_type))
      {
        kh_key(maptype, iter).n_types = 1;
        kh_key(maptype, iter).type_index =
          GetSourceMapTypeRef(env, unit, maptype, n_types, innertype.getValue(), mapname, n_names, map);
      }
      break;
    default: return (size_t)~0;
    }
  }

  return kh_value(maptype, iter);
}

bool ParseDWARFChild(Environment* env, DWARFContext& DICtx, SourceMap* map, SourceMapScope* parent, const DWARFDie& die,
                     llvm::DWARFUnit* CU, size_t& n_variables, size_t& n_ranges, size_t& n_scopes, size_t& n_functions,
                     kh_maptype_t* maptype, size_t& n_types, kh_mapname_t* mapname, size_t& n_names,
                     size_t code_section_offset)
{
  if(die.getTag() == DW_TAG_variable || die.getTag() == DW_TAG_formal_parameter)
  {
    assert(parent != 0);
    parent->variables[parent->n_variables++] = n_variables;
    auto& v                                  = map->x_innative_variables[n_variables];
    v.offset                                 = die.getOffset();
    v.type_index                             = (size_t)~0;
    v.source_index                           = (size_t)~0;
    v.name_index                             = (size_t)~0;
    v.tag                                    = die.getTag();

    // Resolve standard attributes for variables or formal parameters
    if(auto name = die.find(DW_AT_name))
    {
      if(auto attr = name->getAsCString())
        v.name_index = GetSourceMapName(env, mapname, attr.getValue(), n_names, map);
    }
    if(auto line = die.find(DW_AT_decl_line))
      v.original_line = line->getAsUnsignedConstant().getValue();
    if(auto col = die.find(DW_AT_decl_column))
      v.original_column = col->getAsUnsignedConstant().getValue();
    if(auto file = die.find(DW_AT_decl_file))
    {
      if(const auto* LT = CU->getContext().getLineTableForUnit(CU))
        v.source_index = file->getAsUnsignedConstant().getValue() - 1;
    }
    if(auto location = die.find(DW_AT_location))
    {
      auto fn_parse_expr = [](Environment* env, llvm::DWARFExpression& expr, SourceMapVariable& v) -> bool {
        v.n_expr = 0;
        for(auto& op : expr)
          v.n_expr += 1 + (op.getDescription().Op[0] != llvm::DWARFExpression::Operation::SizeNA) +
                      (op.getDescription().Op[1] != llvm::DWARFExpression::Operation::SizeNA);

        v.p_expr = innative::utility::tmalloc<long long>(*env, v.n_expr);
        if(!v.p_expr)
          return false;

        v.n_expr = 0;
        for(auto& op : expr)
        {
          v.p_expr[v.n_expr++] = op.getCode();
          for(int i = 0; i < 2; ++i)
            if(op.getDescription().Op[i] != llvm::DWARFExpression::Operation::SizeNA)
              v.p_expr[v.n_expr++] = op.getRawOperand(i);
        }

        return true;
      };

      if(Optional<llvm::ArrayRef<uint8_t>> block = location->getAsBlock())
      {
        llvm::DataExtractor data(llvm::toStringRef(*block), DICtx.isLittleEndian(), 0);
        llvm::DWARFExpression expr(data, CU->getVersion(), CU->getAddressByteSize());
        fn_parse_expr(env, expr, v);
      }
      else if(Optional<uint64_t> Offset = location->getAsSectionOffset())
      {
        // Location list.
        if(const DWARFDebugLoc* DebugLoc = DICtx.getDebugLoc())
        {
          if(const DWARFDebugLoc::LocationList* LocList = DebugLoc->getLocationListAtOffset(*Offset))
          {
            for(auto& entry : LocList->Entries)
            {
              if(entry.Loc.size() > 0)
              {
                llvm::DataExtractor data(StringRef(entry.Loc.data(), entry.Loc.size()), DICtx.isLittleEndian(), 0);
                llvm::DWARFExpression expr(data, CU->getVersion(), CU->getAddressByteSize());
                fn_parse_expr(env, expr, v);
              }
            }
          }
        }
      }
    }

    if(auto type = die.find(DW_AT_type))
      v.type_index = GetSourceMapTypeRef(env, *CU, maptype, n_types, type.getValue(), mapname, n_names, map);

    ++n_variables;
  }
  else
  {
    SourceMapScope* scope = 0;
    if(die.getTag() == DW_TAG_lexical_block)
    {
      scope = &map->x_innative_scopes[n_scopes];
      if(auto addresses = die.getAddressRanges())
      {
        for(auto& a : addresses.get())
        {
          assert(a.SectionIndex == ~0ULL);
          if(a.valid() && a.LowPC != 0)
            map->x_innative_ranges[n_ranges++] =
              SourceMapRange{ n_scopes, (size_t)a.LowPC + code_section_offset, (size_t)a.HighPC + code_section_offset };
        }
      }
      ++n_scopes;
    }
    else
    {
      auto& v = map->x_innative_functions[n_functions];
      scope   = &v.scope;

      if(auto line = die.find(DW_AT_decl_line))
        v.original_line = line->getAsUnsignedConstant().getValue();
      if(auto type = die.find(DW_AT_type))
        v.type_index = GetSourceMapTypeRef(env, *CU, maptype, n_types, type.getValue(), mapname, n_names, map);
      if(auto file = die.find(DW_AT_decl_file))
      {
        if(const auto* LT = CU->getContext().getLineTableForUnit(CU))
          v.source_index = file->getAsUnsignedConstant().getValue() - 1;
      }

      if(auto addresses = die.getAddressRanges())
      {
        for(auto& a : addresses.get())
        {
          assert(a.SectionIndex == ~0ULL);
          if(a.valid())
            map->x_innative_functions[n_functions].range = SourceMapRange{ die.getOffset(),
                                                                           (size_t)a.LowPC + code_section_offset,
                                                                           (size_t)a.HighPC + code_section_offset };
        }
      }

      ++n_functions;
    }

    scope->name_index = (size_t)~0;
    if(auto name = die.find(DW_AT_name))
    {
      if(auto attr = name->getAsCString())
        scope->name_index = GetSourceMapName(env, mapname, attr.getValue(), n_names, map);
    }

    scope->n_variables = 0;
    for(auto& child : die.children())
      scope->n_variables += child.getTag() == DW_TAG_variable || child.getTag() == DW_TAG_formal_parameter;

    scope->variables   = innative::utility::tmalloc<size_t>(*env, scope->n_variables);
    scope->n_variables = 0;

    if(!scope->variables)
      return false;

    for(auto& child : die.children())
      if(!ParseDWARFChild(env, DICtx, map, scope, child, CU, n_variables, n_ranges, n_scopes, n_functions, maptype, n_types,
                          mapname, n_names, code_section_offset))
        return false;
  }

  return true;
}

bool DumpSourceMap(Environment* env, DWARFContext& DICtx, SourceMap* map, size_t code_section_offset)
{
  auto mapname   = kh_init_mapname();
  auto maptype   = kh_init_maptype();
  size_t n_names = map->n_names;
  size_t n_types = map->n_innative_types;

  for(auto& CU : DICtx.compile_units())
  {
    size_t file_offset    = map->n_sources;
    size_t content_offset = map->n_sourcesContent;
    size_t mapping_offset = map->n_segments;

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

    resizeSourceMap(env, map->segments, map->n_segments, mapping_offset + linetable->Rows.size());
    if(!map->segments)
      return false;

    // If clang encounters an unused function that wasn't removed (because you compiled in debug mode), it generates
    // invalid debug information by restarting at address 0x0, so if we detect this, we skip the rest of the line table.
    bool first = false;
    for(auto& row : linetable->Rows)
    {
      if(!first) // record first non-zero address
        first = row.Address.Address != 0;
      else if(!row.Address.Address)
        break; // Bail if this linetable is invalid

      if(!row.Line)
        continue;
      map->segments[mapping_offset].linecolumn      = row.Address.Address + code_section_offset;
      map->segments[mapping_offset].original_column = row.Column;
      map->segments[mapping_offset].original_line   = row.Line;
      map->segments[mapping_offset].source_index    = row.File - 1 + file_offset;

      ++mapping_offset;
      assert(mapping_offset <= map->n_segments);
    }

    map->n_segments      = mapping_offset;
    size_t variablecount = 0;
    size_t globalcount   = 0;
    size_t scopecount    = 0;
    size_t rangecount    = 0;
    size_t functioncount = 0;

    for(auto& die : CU->dies())
    {
      variablecount += die.getTag() == DW_TAG_variable || die.getTag() == DW_TAG_formal_parameter;
      globalcount += die.getTag() == DW_TAG_variable && die.getDepth() == 1;
      if(die.getTag() == DW_TAG_lexical_block) // || DW_TAG_inlined_subroutine
        if(auto addresses = DWARFDie(CU.get(), &die).getAddressRanges())
          rangecount += addresses->size();
      scopecount += die.getTag() == DW_TAG_lexical_block; // || DW_TAG_inlined_subroutine
      functioncount += die.getTag() == DW_TAG_subprogram;
    }

    size_t n_variables = map->n_innative_variables;
    resizeSourceMap(env, map->x_innative_variables, map->n_innative_variables, n_variables + variablecount);
    size_t n_globals = map->n_innative_globals;
    resizeSourceMap(env, map->x_innative_globals, map->n_innative_globals, n_globals + globalcount);
    size_t n_ranges = map->n_innative_ranges;
    resizeSourceMap(env, map->x_innative_ranges, map->n_innative_ranges, n_ranges + rangecount);
    size_t n_scopes = map->n_innative_scopes;
    resizeSourceMap(env, map->x_innative_scopes, map->n_innative_scopes, n_scopes + scopecount);
    size_t n_functions = map->n_innative_functions;
    resizeSourceMap(env, map->x_innative_functions, map->n_innative_functions, n_functions + functioncount);

    if((!map->x_innative_variables && map->n_innative_variables) || (!map->x_innative_ranges && map->n_innative_ranges) ||
       (!map->x_innative_scopes && map->n_innative_scopes) || (!map->x_innative_functions && map->n_innative_functions) ||
       (!map->x_innative_globals && map->n_innative_globals))
      return false;

    SourceMapScope global_scope = { 0, n_globals, map->x_innative_globals };

    for(auto& entry : CU->dies())
    {
      auto die = DWARFDie(CU.get(), &entry);

      if(die.getTag() == DW_TAG_subprogram || (die.getTag() == DW_TAG_variable && entry.getDepth() == 1))
        if(!ParseDWARFChild(env, DICtx, map, &global_scope, die, CU.get(), n_variables, n_ranges, n_scopes, n_functions,
                            maptype, n_types, mapname, n_names, code_section_offset))
          return false;
    }

    map->n_innative_globals   = global_scope.n_variables;
    map->n_innative_variables = n_variables;
    map->n_innative_functions = n_functions;
    map->n_innative_scopes    = n_scopes;
    map->n_innative_ranges    = n_ranges;
  }

  /*resizeSourceMap(env, map->names, map->n_names, n_names);
  for(khint_t i = 0; i < kh_end(mapname); ++i)
    if(kh_exist(mapname, i))
      map->names[kh_value(mapname, i)] = kh_key(mapname, i);*/
  map->n_names = n_names;
  kh_destroy_mapname(mapname);

  resizeSourceMap(env, map->x_innative_types, map->n_innative_types, n_types);
  for(khint_t i = 0; i < kh_end(maptype); ++i)
    if(kh_exist(maptype, i))
      map->x_innative_types[kh_value(maptype, i)] = kh_key(maptype, i);
  kh_destroy_maptype(maptype);

  std::sort(map->segments, map->segments + map->n_segments,
            [](SourceMapSegment& a, SourceMapSegment& b) { return a.linecolumn < b.linecolumn; });
  std::sort(map->x_innative_ranges, map->x_innative_ranges + map->n_innative_ranges,
            [](SourceMapRange& a, SourceMapRange& b) { return a.low == b.low ? a.high > b.high : a.low < b.low; });
  std::sort(map->x_innative_functions, map->x_innative_functions + map->n_innative_functions,
            [](SourceMapFunction& a, SourceMapFunction& b) { return a.range.low < b.range.low; });

  map->x_google_linecount = !map->n_segments ? 0 : (map->segments[map->n_segments - 1].linecolumn >> 32) + 1;
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