// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "dwarf_parser.h"
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

using namespace innative;

__KHASH_IMPL(mapname, , const char*, size_t, 1, kh_str_hash_func, kh_str_hash_equal)
__KHASH_IMPL(maptype, , SourceMapType*, size_t, 1, kh_type_hash, kh_type_equal)

bool DWARFParser::error(llvm::StringRef Prefix, std::error_code EC)
{
  if(!EC)
    return true;
  fwrite(Prefix.begin(), 1, Prefix.size(), env->log);
  return false;
}

bool DWARFParser::handleArchive(StringRef Filename, Archive& Arch, HandlerFn HandleObj)
{
  bool Result = true;
  Error Err   = Error::success();
  for(auto Child : Arch.children(Err))
  {
    auto BuffOrErr = Child.getMemoryBufferRef();
    if(!error(Filename, errorToErrorCode(BuffOrErr.takeError())))
      return false;
    auto NameOrErr = Child.getName();
    if(!error(Filename, errorToErrorCode(NameOrErr.takeError())))
      return false;
    std::string Name = (Filename + "(" + NameOrErr.get() + ")").str();
    Result &= handleBuffer(Name, BuffOrErr.get(), HandleObj);
  }
  return Result && error(Filename, errorToErrorCode(std::move(Err)));
}

bool DWARFParser::handleBuffer(StringRef Filename, MemoryBufferRef Buffer, HandlerFn HandleObj)
{
  size_t code_section_offset = 0;
  if(Buffer.getBufferSize() > 8)
  {
    innative::utility::Stream s = { reinterpret_cast<const uint8_t*>(Buffer.getBufferStart()), Buffer.getBufferSize(), 0 };

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
  if(!error(Filename, errorToErrorCode(BinOrErr.takeError())))
    return false;

  bool Result = true;
  if(auto* Obj = dyn_cast<ObjectFile>(BinOrErr->get()))
  {
    std::unique_ptr<DWARFContext> DICtx = DWARFContext::create(*Obj);
    Result                              = (this->*HandleObj)(*DICtx, code_section_offset);
  }
  else if(auto* Fat = dyn_cast<MachOUniversalBinary>(BinOrErr->get()))
    for(auto& ObjForArch : Fat->objects())
    {
      std::string ObjName = (Filename + "(" + ObjForArch.getArchFlagName() + ")").str();
      if(auto MachOOrErr = ObjForArch.getAsObjectFile())
      {
        auto& Obj                           = **MachOOrErr;
        std::unique_ptr<DWARFContext> DICtx = DWARFContext::create(Obj);
        Result &= (this->*HandleObj)(*DICtx, code_section_offset);
        continue;
      }
      else
        consumeError(MachOOrErr.takeError());
      if(auto ArchiveOrErr = ObjForArch.getAsArchive())
      {
        if(!error(ObjName, errorToErrorCode(ArchiveOrErr.takeError())))
          return false;
        Result &= handleArchive(ObjName, *ArchiveOrErr.get(), HandleObj);
        continue;
      }
      else
        consumeError(ArchiveOrErr.takeError());
    }
  else if(auto* Arch = dyn_cast<Archive>(BinOrErr->get()))
    Result = handleArchive(Filename, *Arch, HandleObj);
  return Result;
}

bool DWARFParser::handleFile(StringRef Filename, HandlerFn HandleObj)
{
  map->file = innative::utility::AllocString(*env, Filename.str());

  llvm::ErrorOr<std::unique_ptr<MemoryBuffer>> BuffOrErr = MemoryBuffer::getFileOrSTDIN(Filename);
  if(!error(Filename, BuffOrErr.getError()))
    return false;
  std::unique_ptr<MemoryBuffer> Buffer = std::move(BuffOrErr.get());
  return handleBuffer(Filename, *Buffer, HandleObj);
}

DWARFDie DWARFParser::AsReferencedDIE(const DWARFFormValue& V, const llvm::DWARFUnit& unit)
{
  if(auto SpecRef = V.getAsRelativeReference())
  {
    if(SpecRef->Unit)
      return SpecRef->Unit->getDIEForOffset(SpecRef->Unit->getOffset() + static_cast<uint32_t>(SpecRef->Offset));
    if(auto SpecUnit = unit.getUnitVector().getUnitForOffset(static_cast<uint32_t>(SpecRef->Offset)))
      return SpecUnit->getDIEForOffset(static_cast<uint32_t>(SpecRef->Offset));
  }
  return DWARFDie();
}

size_t DWARFParser::GetSourceMapName(const char* name)
{
  auto iter = kh_get_mapname(mapname, name);
  if(iter >= kh_end(mapname))
  {
    int r;
    iter = kh_put_mapname(mapname, innative::utility::AllocString(*env, name), &r);
    if(r < 0)
      return (size_t)~0;
    kh_value(mapname, iter) = n_names++;

    if(n_names > map->n_names)
      resizeSourceMap(map->names, map->n_names, n_names * 2);
    map->names[n_names - 1] = kh_key(mapname, iter);
  }
  return kh_value(mapname, iter);
};

void DWARFParser::ResolveDWARFBitSize(const DWARFDie& die, SourceMapType* ptype)
{
  return ResolveBitAttribute<DW_AT_bit_size, DW_AT_byte_size>(die, ptype->bit_size);
}

void DWARFParser::ResolveDWARFTypeFlags(const DWARFDie& die, SourceMapType* ptype)
{
  if(auto flag = die.find(DW_AT_accessibility))
  {
    switch(flag->getAsUnsignedConstant().getValueOr(0ULL))
    {
    case DW_ACCESS_public: ptype->flags |= IN_SOURCE_TYPE_PUBLIC; break;
    case DW_ACCESS_private: ptype->flags |= IN_SOURCE_TYPE_PRIVATE; break;
    case DW_ACCESS_protected: ptype->flags |= IN_SOURCE_TYPE_PROTECTED; break;
    }
  }
  if(auto flag = die.find(DW_AT_virtuality))
  {
    switch(flag->getAsUnsignedConstant().getValueOr(0ULL))
    {
    case DW_VIRTUALITY_virtual: ptype->flags |= IN_SOURCE_TYPE_VIRTUAL; break;
    case DW_VIRTUALITY_pure_virtual: ptype->flags |= IN_SOURCE_TYPE_PURE_VIRTUAL; break;
    }
  }
  if(auto flag = die.find(DW_AT_friend))
    ptype->flags |= IN_SOURCE_TYPE_FRIEND;
  if(auto flag = die.find(DW_AT_artificial))
    ptype->flags |= IN_SOURCE_TYPE_ARTIFICIAL;
  if(auto flag = die.find(DW_AT_declaration))
    ptype->flags |= IN_SOURCE_TYPE_DECLARATION;
  if(auto flag = die.find(DW_AT_default_value))
    ptype->flags |= IN_SOURCE_TYPE_DEFAULT;
  if(auto flag = die.find(DW_AT_export_symbols))
    ptype->flags |= IN_SOURCE_TYPE_EXPORT;
  if(auto flag = die.find(DW_AT_mutable))
    ptype->flags |= IN_SOURCE_TYPE_MUTABLE;
  if(auto flag = die.find(DW_AT_enum_class))
    ptype->flags |= IN_SOURCE_TYPE_ENUM_CLASS;
}

size_t DWARFParser::GetSourceMapTypeRef(llvm::DWARFUnit& unit, const DWARFFormValue& type)
{
  if(DWARFDie die = AsReferencedDIE(type, unit))
    return GetSourceMapType(unit, die);
  return (size_t)~0;
}

const char* DWARFParser::GetDieName(const DWARFDie& die)
{
  SourceMapType key = { die.getOffset() };
  if(auto iter = kh_get_maptype(maptype, &key); kh_exist2(maptype, iter))
    if(kh_key(maptype, iter)->name_index < n_names)
      return map->names[kh_key(maptype, iter)->name_index];
  return 0;
}

size_t DWARFParser::GetSourceMapType(llvm::DWARFUnit& unit, const DWARFDie& die)
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
  case DW_TAG_unspecified_parameters:
  case DW_TAG_member:
  case DW_TAG_inheritance:
  case DW_TAG_typedef: break;
  default: return (size_t)~0;
  }

  SourceMapType key = { die.getOffset() };
  auto iter         = kh_get_maptype(maptype, &key);
  if(!kh_exist2(maptype, iter))
  {
    // This can waste a significant chunk of memory for large debugging payloads, but trying to keep track
    // of the hash iterator while modifying itself is way too error-prone.
    SourceMapType* ptype = utility::tmalloc<SourceMapType>(*env, 1);
    ptype->offset        = die.getOffset();
    int r;
    if(iter = kh_put_maptype(maptype, ptype, &r); r < 0)
      return (size_t)~0;

    kh_value(maptype, iter) = n_types++;
    ptype->tag              = die.getTag();
    ptype->bit_size         = 0;
    ptype->byte_align       = 0;
    ptype->name_index       = (size_t)~0;
    ptype->source_index     = (size_t)~0;
    ptype->type_index       = (size_t)~0;
    ptype->n_types          = 0;

    if(auto file = die.find(DW_AT_decl_file))
    {
      if(const auto* LT = unit.getContext().getLineTableForUnit(&unit))
        ptype->source_index = static_cast<size_t>(file->getAsUnsignedConstant().getValueOr(0ULL) - 1) + file_offset;
    }
    if(auto line = die.find(DW_AT_decl_line))
      ptype->original_line = static_cast<unsigned int>(line->getAsUnsignedConstant().getValueOr(0ULL));
    if(auto name = die.find(DW_AT_name))
    {
      if(auto attr = name->getAsCString())
        ptype->name_index = GetSourceMapName(attr.getValue());
    }
    if(auto align = die.find(DW_AT_alignment))
      ptype->byte_align = static_cast<unsigned short>(align->getAsUnsignedConstant().getValueOr(0ULL));
    ResolveDWARFTypeFlags(die, ptype);

    // Figure out what kind of type this is
    switch(die.getTag())
    {
    case DW_TAG_base_type:
      ptype->n_types  = 0;
      ptype->encoding = 0;
      ResolveDWARFBitSize(die, ptype);

      if(auto line = die.find(DW_AT_encoding))
        ptype->encoding = static_cast<unsigned short>(line->getAsUnsignedConstant().getValueOr(0ULL));

      break;
    case DW_TAG_array_type:
      ResolveBitAttribute<DW_AT_bit_stride, DW_AT_byte_stride>(die, ptype->bit_stride);
      ResolveDWARFBitSize(die, ptype);

      for(auto& child : die.children())
        if(child.getTag() == DW_TAG_subrange_type)
        {
          if(auto count = child.find(DW_AT_count))
            ptype->n_types = count->getAsUnsignedConstant().getValueOr(0ULL);
        }
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
      if(!ptype->n_types)
        ptype->n_types = 1;
      const char* basename = "void";

      if(auto innertype = die.find(DW_AT_type))
      {
        if(DWARFDie innerdie = AsReferencedDIE(innertype.getValue(), unit))
        {
          ptype->type_index = GetSourceMapType(unit, innerdie);
          if(auto diename = GetDieName(innerdie); diename != nullptr)
            basename = diename;
        }
      } // If this is false, it's a void type, like const void*

      if(ptype->name_index == (size_t)~0)
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
        case DW_TAG_array_type: modifier = modifier + "[" + std::to_string(ptype->n_types) + "]"; break;
        }
        ptype->name_index = GetSourceMapName(modifier.c_str());
      }

      break;
    }
    case DW_TAG_enumeration_type:
    {
      ResolveDWARFBitSize(die, ptype);
      if(auto enumtype = die.find(DW_AT_type))
        ptype->type_index = GetSourceMapTypeRef(unit, enumtype.getValue());

      size_t count = 0;
      for(auto& child : die.children())
        count += child.getTag() == DW_TAG_enumerator;

      ptype->enumerators   = innative::utility::tmalloc<size_t>(*env, count);
      size_t n_enumerators = map->n_innative_enumerators;
      resizeSourceMap(map->x_innative_enumerators, map->n_innative_enumerators, n_enumerators + count);

      for(auto& child : die.children())
      {
        if(child.getTag() != DW_TAG_enumerator)
          continue;

        auto& e = map->x_innative_enumerators[n_enumerators];
        if(auto value = child.find(DW_AT_const_value))
          if(auto constant = value->getAsUnsignedConstant())
            e.val = constant.getValue();
          else
            e.val = static_cast<uint64_t>(value->getAsSignedConstant().getValueOr(0LL));

        if(auto name = child.find(DW_AT_name))
        {
          if(auto attr = name->getAsCString())
            e.name_index = GetSourceMapName(attr.getValue());
        }

        ptype->enumerators[ptype->n_types++] = n_enumerators++;
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
          ptype->type_index = GetSourceMapType(unit, innerdie);
          if(auto diename = GetDieName(innerdie); diename != nullptr)
            funcptr = diename;
        }
      }
      funcptr += '(';

      size_t count = 0;
      for(auto& child : die.children())
        count++;

      ptype->types = innative::utility::tmalloc<size_t>(*env, count);
      if(!ptype->types)
        return (size_t)~0;

      bool first = true;
      for(auto& child : die.children())
      {
        if(!first)
          funcptr += ", ";
        auto ty = GetSourceMapType(unit, child);
        if(ty != (size_t)~0)
          ptype->types[ptype->n_types++] = ty;

        if(auto innertype = child.find(DW_AT_type))
          if(DWARFDie innerdie = AsReferencedDIE(innertype.getValue(), unit))
            if(auto diename = GetDieName(innerdie); diename != nullptr)
              funcptr += diename;

        if(auto diename = GetDieName(child); diename != nullptr)
        {
          funcptr += ' ';
          funcptr += diename;
        }
      }
      funcptr += ')';

      if(ptype->name_index == (size_t)~0)
        ptype->name_index = GetSourceMapName(funcptr.c_str());
      break;
    }
    case DW_TAG_class_type:
    case DW_TAG_structure_type:
    case DW_TAG_union_type:
    case DW_TAG_interface_type:
    {
      ResolveDWARFBitSize(die, ptype);

      size_t count = 0;
      for(auto& child : die.children())
        count++;

      ptype->types = innative::utility::tmalloc<size_t>(*env, count);
      if(!ptype->types)
        return (size_t)~0;

      for(auto& child : die.children())
      {
        auto ty = GetSourceMapType(unit, child);
        if(ty != (size_t)~0)
          ptype->types[ptype->n_types++] = ty;
      }
      break;
    }
    case DW_TAG_formal_parameter:
    case DW_TAG_template_value_parameter:
    case DW_TAG_template_type_parameter:
      if(auto innertype = die.find(DW_AT_type))
      {
        ptype->n_types    = 1;
        ptype->type_index = GetSourceMapTypeRef(unit, innertype.getValue());
      }
    case DW_TAG_unspecified_parameters: break;
    case DW_TAG_member:
    case DW_TAG_inheritance:
      ResolveDWARFBitSize(die, ptype);
      ResolveBitAttribute<DW_AT_bit_offset, DW_AT_data_member_location>(die, ptype->bit_offset);
      if(auto innertype = die.find(DW_AT_type))
      {
        ptype->n_types    = 1;
        ptype->type_index = GetSourceMapTypeRef(unit, innertype.getValue());
      }
      break;
    default: return (size_t)~0;
    }
  }

  return kh_value(maptype, iter);
}

bool DWARFParser::ParseDWARFChild(DWARFContext& DICtx, SourceMapScope* parent, const DWARFDie& die, llvm::DWARFUnit* CU,
                                  size_t code_section_offset)
{
  auto tag = die.getTag();
  if(tag == DW_TAG_variable || tag == DW_TAG_formal_parameter || tag == DW_TAG_unspecified_parameters)
  {
    assert(parent != 0);
    assert(n_variables < map->n_innative_variables);
    parent->variables[parent->n_variables++] = n_variables;
    auto& v                                  = map->x_innative_variables[n_variables];
    v.offset                                 = die.getOffset();
    v.type_index                             = (size_t)~0;
    v.source_index                           = (size_t)~0;
    v.name_index                             = (size_t)~0;
    v.tag                                    = tag;

    // Resolve standard attributes for variables or formal parameters
    if(auto name = die.find(DW_AT_name))
    {
      if(auto attr = name->getAsCString())
        v.name_index = GetSourceMapName(attr.getValue());
    }
    if(auto line = die.find(DW_AT_decl_line))
      v.original_line = static_cast<decltype(v.original_line)>(line->getAsUnsignedConstant().getValue());
    if(auto col = die.find(DW_AT_decl_column))
      v.original_column = static_cast<decltype(v.original_column)>(col->getAsUnsignedConstant().getValue());
    if(auto file = die.find(DW_AT_decl_file))
    {
      if(const auto* LT = CU->getContext().getLineTableForUnit(CU))
        v.source_index = static_cast<decltype(v.source_index)>(file->getAsUnsignedConstant().getValue() - 1) + file_offset;
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
      v.type_index = GetSourceMapTypeRef(*CU, type.getValue());

    ++n_variables;
  }
  else
  {
    SourceMapScope* scope = 0;
    if(tag == DW_TAG_lexical_block)
    {
      assert(n_functions < map->n_innative_scopes);
      scope = &map->x_innative_scopes[n_scopes];
      if(auto addresses = die.getAddressRanges())
      {
        for(auto& a : addresses.get())
        {
          assert(a.SectionIndex == ~0ULL);
          if(a.valid() && a.LowPC != 0)
          {
            assert(n_ranges < map->n_innative_ranges);
            map->x_innative_ranges[n_ranges++] =
              SourceMapRange{ n_scopes, (size_t)a.LowPC + code_section_offset, (size_t)a.HighPC + code_section_offset };
          }
        }
      }
      ++n_scopes;
    }
    else if(tag == DW_TAG_subprogram)
    {
      assert(n_functions < map->n_innative_functions);
      auto& v = map->x_innative_functions[n_functions];
      assert(n_scopes < map->n_innative_scopes);
      scope = &map->x_innative_scopes[n_scopes];

      map->x_innative_functions[n_functions].range.scope = n_scopes;

      if(auto line = die.find(DW_AT_decl_line))
        v.original_line = static_cast<decltype(v.original_line)>(line->getAsUnsignedConstant().getValue());
      if(auto type = die.find(DW_AT_type))
        v.type_index = GetSourceMapTypeRef(*CU, type.getValue());
      if(auto file = die.find(DW_AT_decl_file))
      {
        if(const auto* LT = CU->getContext().getLineTableForUnit(CU))
          v.source_index =
            static_cast<decltype(v.source_index)>(file->getAsUnsignedConstant().getValue() - 1) + file_offset;
      }

      if(auto addresses = die.getAddressRanges())
      {
        for(auto& a : addresses.get())
        {
          assert(a.SectionIndex == ~0ULL);
          if(a.valid())
            map->x_innative_functions[n_functions].range =
              SourceMapRange{ n_scopes, (size_t)a.LowPC + code_section_offset, (size_t)a.HighPC + code_section_offset };
        }
      }

      ++n_scopes;
      ++n_functions;
    }
    else if(tag == DW_TAG_label)
      return true; // Ignored
    else if(tag == DW_TAG_inlined_subroutine)
      return true; // not currently handled
    else
      return true; // We don't throw an error here so unexpected debug entries can simply be ignored

    scope->name_index = (size_t)~0;
    if(auto name = die.find(DW_AT_name))
    {
      if(auto attr = name->getAsCString())
        scope->name_index = GetSourceMapName(attr.getValue());
    }

    scope->n_variables = 0;
    for(auto& child : die.children())
      scope->n_variables += child.getTag() == DW_TAG_variable || child.getTag() == DW_TAG_formal_parameter ||
                            child.getTag() == DW_TAG_unspecified_parameters;

    scope->variables   = innative::utility::tmalloc<size_t>(*env, scope->n_variables);
    scope->n_variables = 0;

    if(!scope->variables)
      return false;

    for(auto& child : die.children())
      if(!ParseDWARFChild(DICtx, scope, child, CU, code_section_offset))
        return false;
  }

  return true;
}

bool DWARFParser::HasIsStmt(size_t i, const llvm::DWARFDebugLine::LineTable::RowVector& rows)
{
  if(env->flags & ENV_DEBUG_IGNORE_IS_STMT)
    return false;
  if(env->flags & ENV_DEBUG_USE_IS_STMT)
    return true;
  if(i >= rows.size())
    return false;

  do
  {
    if(rows[i].IsStmt != 0) // The first line of a function could have the EndSequence marker for the previous function set.
      return true;
    ++i;
  } while(i < rows.size() && !rows[i].EndSequence);

  return false;
}

bool DWARFParser::DumpSourceMap(DWARFContext& DICtx, size_t code_section_offset)
{
  for(auto& CU : DICtx.compile_units())
  {
    file_offset    = map->n_sources;
    content_offset = map->n_sourcesContent;
    mapping_offset = map->n_segments;

    auto linetable  = DICtx.getLineTableForUnit(CU.get());
    auto& filenames = linetable->Prologue.FileNames;

    resizeSourceMap(map->sources, map->n_sources, map->n_sources + filenames.size());
    resizeSourceMap(map->sourcesContent, map->n_sourcesContent, map->n_sourcesContent + filenames.size());
    if(!map->sources || !map->sourcesContent)
      return false;

    for(size_t i = 0; i < filenames.size(); ++i)
    {
      map->sources[file_offset + i] = "";
      std::string File;
      if(linetable->getFileNameByIndex(i + 1, CU->getCompilationDir(),
                                       llvm::DILineInfoSpecifier::FileLineInfoKind::AbsoluteFilePath, File))
      {
        map->sources[file_offset + i] = innative::utility::AllocString(*env, absolute(File).u8string());
      }

      map->sourcesContent[content_offset + i] =
        filenames[i].Source.getAsCString().hasValue() ?
          innative::utility::AllocString(*env, filenames[i].Source.getAsCString().getValue()) :
          "";
    }

    resizeSourceMap(map->segments, map->n_segments, mapping_offset + linetable->Rows.size());
    if(!map->segments)
      return false;

    // If clang encounters an unused function that wasn't removed (because you compiled in debug mode), it generates
    // invalid debug information by restarting at address 0x0, so if we detect this, we skip to the next function.
    bool skip = false;
    bool only_stmt = HasIsStmt(0, linetable->Rows);
    for(size_t i = 0; i < linetable->Rows.size(); ++i)
    {
      auto& row = linetable->Rows[i];
      if(row.EndSequence)
        only_stmt = HasIsStmt(i, linetable->Rows);

      if(skip)
        skip = (row.EndSequence != 0);
      else
        skip = (row.Address.Address == 0);

      if(skip || !row.Line)
        continue;
      if(only_stmt && !row.IsStmt)
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
      variablecount += die.getTag() == DW_TAG_variable || die.getTag() == DW_TAG_formal_parameter ||
                       die.getTag() == DW_TAG_unspecified_parameters;
      globalcount += die.getTag() == DW_TAG_variable && die.getDepth() == 1;
      if(die.getTag() == DW_TAG_lexical_block) // || DW_TAG_inlined_subroutine
        if(auto addresses = DWARFDie(CU.get(), &die).getAddressRanges())
          rangecount += addresses->size();
      scopecount += die.getTag() == DW_TAG_lexical_block ||
                    die.getTag() == DW_TAG_subprogram; // || DW_TAG_inlined_subroutine
      functioncount += die.getTag() == DW_TAG_subprogram;
    }

    size_t n_globals = map->n_innative_globals;
    resizeSourceMap(map->x_innative_globals, map->n_innative_globals, n_globals + globalcount);
    n_variables = map->n_innative_variables;
    resizeSourceMap(map->x_innative_variables, map->n_innative_variables, n_variables + variablecount);
    n_ranges = map->n_innative_ranges;
    resizeSourceMap(map->x_innative_ranges, map->n_innative_ranges, n_ranges + rangecount);
    n_scopes = map->n_innative_scopes;
    resizeSourceMap(map->x_innative_scopes, map->n_innative_scopes, n_scopes + scopecount);
    n_functions = map->n_innative_functions;
    resizeSourceMap(map->x_innative_functions, map->n_innative_functions, n_functions + functioncount);

    if((!map->x_innative_variables && map->n_innative_variables) || (!map->x_innative_ranges && map->n_innative_ranges) ||
       (!map->x_innative_scopes && map->n_innative_scopes) || (!map->x_innative_functions && map->n_innative_functions) ||
       (!map->x_innative_globals && map->n_innative_globals))
      return false;

    SourceMapScope global_scope = { 0, n_globals, map->x_innative_globals };

    for(auto& entry : CU->dies())
    {
      auto die = DWARFDie(CU.get(), &entry);

      if(die.getTag() == DW_TAG_subprogram || (die.getTag() == DW_TAG_variable && entry.getDepth() == 1))
        if(!ParseDWARFChild(DICtx, &global_scope, die, CU.get(), code_section_offset))
          return false;
    }

    map->n_innative_globals   = global_scope.n_variables;
    map->n_innative_variables = n_variables;
    map->n_innative_functions = n_functions;
    map->n_innative_scopes    = n_scopes;
    map->n_innative_ranges    = n_ranges;
  }

  map->n_names = n_names;

  resizeSourceMap(map->x_innative_types, map->n_innative_types, n_types);
  for(khint_t i = 0; i < kh_end(maptype); ++i)
    if(kh_exist(maptype, i))
      map->x_innative_types[kh_value(maptype, i)] = *kh_key(maptype, i);

  std::sort(map->segments, map->segments + map->n_segments,
            [](SourceMapSegment& a, SourceMapSegment& b) { return a.linecolumn < b.linecolumn; });
  std::sort(map->x_innative_ranges, map->x_innative_ranges + map->n_innative_ranges,
            [](SourceMapRange& a, SourceMapRange& b) { return a.low == b.low ? a.high > b.high : a.low < b.low; });
  std::sort(map->x_innative_functions, map->x_innative_functions + map->n_innative_functions,
            [](SourceMapFunction& a, SourceMapFunction& b) { return a.range.low < b.range.low; });

  map->x_google_linecount = !map->n_segments ? 0 : (map->segments[map->n_segments - 1].linecolumn >> 32) + 1;
  return true;
}

DWARFParser::DWARFParser(struct IN_WASM_ENVIRONMENT* env, SourceMap* map) :
  env(env),
  map(map),
  n_names(map->n_names),
  n_types(map->n_innative_types),
  n_variables(0),
  n_ranges(0),
  n_scopes(0),
  n_functions(0),
  mapname(kh_init_mapname()),
  maptype(kh_init_maptype()),
  file_offset(0),
  content_offset(0),
  mapping_offset(0)
{}

DWARFParser::~DWARFParser()
{
  kh_destroy_mapname(mapname);
  kh_destroy_maptype(maptype);
}

enum IN_ERROR DWARFParser::ParseDWARF(const char* obj, size_t len)
{
  bool success = true;
  map->version = 3;
  if(!len)
    success = handleFile(obj, &DWARFParser::DumpSourceMap);
  else
    success =
      handleBuffer("memorybuf", MemoryBufferRef(llvm::StringRef(obj, len), "memorybuf"), &DWARFParser::DumpSourceMap);
  return success ? ERR_SUCCESS : ERR_FATAL_FILE_ERROR;
}

enum IN_ERROR ParseDWARF(struct IN_WASM_ENVIRONMENT* env, SourceMap* map, const char* obj, size_t len)
{
  DWARFParser parser(env, map);
  return parser.ParseDWARF(obj, len);
}