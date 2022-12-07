// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

// Most of this file adapted from the llvm-nm tool, under the Apache License v2.0 with LLVM Exceptions
// See https://llvm.org/LICENSE.txt for license information.

#include "llvm.h"
#include "llvm/Object/COFF.h"
#include "llvm/Object/COFFImportFile.h"
#include "llvm/Object/ELFObjectFile.h"
#include "llvm/Object/IRObjectFile.h"
#include "llvm/Object/MachO.h"
#include "llvm/Object/TapiFile.h"
#include "llvm/Object/TapiUniversal.h"
#include "llvm/Object/Wasm.h"
#include "llvm/Object/XCOFFObjectFile.h"
#include "llvm/Support/raw_ostream.h"
#include "dump_symbols.h"

using namespace llvm;
using namespace object;

static char getSymbolNMTypeChar(ELFObjectFileBase& Obj, basic_symbol_iterator I)
{
  // OK, this is ELF
  elf_symbol_iterator SymI(I);

  Expected<elf_section_iterator> SecIOrErr = SymI->getSection();
  if(!SecIOrErr)
  {
    consumeError(SecIOrErr.takeError());
    return '?';
  }

  uint8_t Binding = SymI->getBinding();
  if(Binding == ELF::STB_GNU_UNIQUE)
    return 'u';

  assert(Binding != ELF::STB_WEAK && "STB_WEAK not tested in calling function");
  if(Binding != ELF::STB_GLOBAL && Binding != ELF::STB_LOCAL)
    return '?';

  elf_section_iterator SecI = *SecIOrErr;
  if(SecI != Obj.section_end())
  {
    uint32_t Type  = SecI->getType();
    uint64_t Flags = SecI->getFlags();
    if(Flags & ELF::SHF_EXECINSTR)
      return 't';
    if(Type == ELF::SHT_NOBITS)
      return 'b';
    if(Flags & ELF::SHF_ALLOC)
      return Flags & ELF::SHF_WRITE ? 'd' : 'r';

    auto NameOrErr = SecI->getName();
    if(!NameOrErr)
    {
      consumeError(NameOrErr.takeError());
      return '?';
    }
    if((*NameOrErr).startswith(".debug"))
      return 'N';
    if(!(Flags & ELF::SHF_WRITE))
      return 'n';
  }

  return '?';
}


static char getSymbolNMTypeChar(COFFObjectFile& Obj, symbol_iterator I)
{
  COFFSymbolRef Symb = Obj.getCOFFSymbol(*I);
  // OK, this is COFF.
  symbol_iterator SymI(I);

  Expected<StringRef> Name = SymI->getName();
  if(!Name)
  {
    consumeError(Name.takeError());
    return '?';
  }

  char Ret = StringSwitch<char>(*Name).StartsWith(".debug", 'N').StartsWith(".sxdata", 'N').Default('?');

  if(Ret != '?')
    return Ret;

  uint32_t Characteristics = 0;
  if(!COFF::isReservedSectionNumber(Symb.getSectionNumber()))
  {
    Expected<section_iterator> SecIOrErr = SymI->getSection();
    if(!SecIOrErr)
    {
      consumeError(SecIOrErr.takeError());
      return '?';
    }
    section_iterator SecI       = *SecIOrErr;
    const coff_section* Section = Obj.getCOFFSection(*SecI);
    Characteristics             = Section->Characteristics;
    if(Expected<StringRef> NameOrErr = Obj.getSectionName(Section))
      if(NameOrErr->startswith(".idata"))
        return 'i';
  }

  switch(Symb.getSectionNumber())
  {
  case COFF::IMAGE_SYM_DEBUG: return 'n';
  default:
    // Check section type.
    if(Characteristics & COFF::IMAGE_SCN_CNT_CODE)
      return 't';
    if(Characteristics & COFF::IMAGE_SCN_CNT_INITIALIZED_DATA)
      return Characteristics & COFF::IMAGE_SCN_MEM_WRITE ? 'd' : 'r';
    if(Characteristics & COFF::IMAGE_SCN_CNT_UNINITIALIZED_DATA)
      return 'b';
    if(Characteristics & COFF::IMAGE_SCN_LNK_INFO)
      return 'i';
    // Check for section symbol.
    if(Symb.isSectionDefinition())
      return 's';
  }

  return '?';
}

// For ELF object files, Set TypeName to the symbol typename, to be printed
// in the 'Type' column of the SYSV format output.
static StringRef getNMTypeName(SymbolicFile& Obj, basic_symbol_iterator I)
{
  if(isa<ELFObjectFileBase>(&Obj))
  {
    elf_symbol_iterator SymI(I);
    return SymI->getELFTypeName();
  }
  return "";
}

static char getSymbolNMTypeChar(TapiFile& Obj, basic_symbol_iterator I) { return 's'; }

static char getSymbolNMTypeChar(WasmObjectFile& Obj, basic_symbol_iterator I)
{
  uint32_t Flags = cantFail(I->getFlags());
  if(Flags & SymbolRef::SF_Executable)
    return 't';
  return 'd';
}

static char getSymbolNMTypeChar(IRObjectFile& Obj, basic_symbol_iterator I)
{
  uint32_t Flags = cantFail(I->getFlags());
  // FIXME: should we print 'b'? At the IR level we cannot be sure if this
  // will be in bss or not, but we could approximate.
  if(Flags & SymbolRef::SF_Executable)
    return 't';
  else if(Triple(Obj.getTargetTriple()).isOSDarwin() && (Flags & SymbolRef::SF_Const))
    return 's';
  else
    return 'd';
}

static char getSymbolNMTypeChar(COFFImportFile& Obj)
{
  switch(Obj.getCOFFImportHeader()->getType())
  {
  case COFF::IMPORT_CODE: return 't';
  case COFF::IMPORT_DATA: return 'd';
  case COFF::IMPORT_CONST: return 'r';
  }
  return '?';
}

static char getSymbolNMTypeChar(XCOFFObjectFile& Obj, symbol_iterator I)
{
  Expected<uint32_t> TypeOrErr = I->getType();
  if(!TypeOrErr)
  {
    //warn(TypeOrErr.takeError(), Obj.getFileName(), "for symbol with index " + Twine(Obj.getSymbolIndex(I->getRawDataRefImpl().p)));
    return '?';
  }

  uint32_t SymType = *TypeOrErr;

  if(SymType == SymbolRef::ST_File)
    return 'f';

  // If the I->getSection() call would return an error, the earlier I->getType()
  // call will already have returned the same error first.
  section_iterator SecIter = cantFail(I->getSection());

  if(SecIter == Obj.section_end())
    return '?';

  if(Obj.isDebugSection(SecIter->getRawDataRefImpl()))
    return 'N';

  if(SecIter->isText())
    return 't';

  if(SecIter->isData())
    return 'd';

  if(SecIter->isBSS())
    return 'b';

  return '?';
}

static bool isObject(SymbolicFile& Obj, basic_symbol_iterator I)
{
  return isa<ELFObjectFileBase>(&Obj) && elf_symbol_iterator(I)->getELFType() == ELF::STT_OBJECT;
}

// Return Posix nm class type tag (single letter), but also set SecName and
// section and name, to be used in format=sysv output.
static char getNMSectionTagAndName(SymbolicFile& Obj, basic_symbol_iterator I, StringRef& SecName)
{
  // Symbol Flags have been checked in the caller.
  uint32_t Symflags = cantFail(I->getFlags());
  if(ELFObjectFileBase* ELFObj = dyn_cast<ELFObjectFileBase>(&Obj))
  {
    if(Symflags & object::SymbolRef::SF_Absolute)
      SecName = "*ABS*";
    else if(Symflags & object::SymbolRef::SF_Common)
      SecName = "*COM*";
    else if(Symflags & object::SymbolRef::SF_Undefined)
      SecName = "*UND*";
    else
    {
      elf_symbol_iterator SymI(I);
      Expected<elf_section_iterator> SecIOrErr = SymI->getSection();
      if(!SecIOrErr)
      {
        consumeError(SecIOrErr.takeError());
        return '?';
      }

      if(*SecIOrErr == ELFObj->section_end())
        return '?';

      Expected<StringRef> NameOrErr = (*SecIOrErr)->getName();
      if(!NameOrErr)
      {
        consumeError(NameOrErr.takeError());
        return '?';
      }
      SecName = *NameOrErr;
    }
  }

  if(Symflags & object::SymbolRef::SF_Undefined)
  {
    if(isa<MachOObjectFile>(Obj) || !(Symflags & object::SymbolRef::SF_Weak))
      return 'U';
    return isObject(Obj, I) ? 'v' : 'w';
  }
  if(isa<ELFObjectFileBase>(&Obj))
    if(ELFSymbolRef(*I).getELFType() == ELF::STT_GNU_IFUNC)
      return 'i';
  if(!isa<MachOObjectFile>(Obj) && (Symflags & object::SymbolRef::SF_Weak))
    return isObject(Obj, I) ? 'V' : 'W';

  if(Symflags & object::SymbolRef::SF_Common)
    return 'C';

  char Ret = '?';
  if(Symflags & object::SymbolRef::SF_Absolute)
    Ret = 'a';
  else if(IRObjectFile* IR = dyn_cast<IRObjectFile>(&Obj))
    Ret = getSymbolNMTypeChar(*IR, I);
  else if(COFFObjectFile* COFF = dyn_cast<COFFObjectFile>(&Obj))
    Ret = getSymbolNMTypeChar(*COFF, I);
  else if(XCOFFObjectFile* XCOFF = dyn_cast<XCOFFObjectFile>(&Obj))
    Ret = getSymbolNMTypeChar(*XCOFF, I);
  else if(COFFImportFile* COFFImport = dyn_cast<COFFImportFile>(&Obj))
    Ret = getSymbolNMTypeChar(*COFFImport);
  else if(WasmObjectFile* Wasm = dyn_cast<WasmObjectFile>(&Obj))
    Ret = getSymbolNMTypeChar(*Wasm, I);
  else if(TapiFile* Tapi = dyn_cast<TapiFile>(&Obj))
    Ret = getSymbolNMTypeChar(*Tapi, I);
  else if(ELFObjectFileBase* ELF = dyn_cast<ELFObjectFileBase>(&Obj))
  {
    Ret = getSymbolNMTypeChar(*ELF, I);
    if(ELFSymbolRef(*I).getBinding() == ELF::STB_GNU_UNIQUE)
      return Ret;
  }
  else
    llvm_unreachable("unknown binary format");

  if(!(Symflags & object::SymbolRef::SF_Global))
    return Ret;

  return toupper(Ret);
}

namespace {
  struct SymbolVersion
  {
    std::string Name;
    bool IsDefault;
  };
}


template<class ELFT>
static Expected<std::vector<SymbolVersion>> readSymbolVersionsELF(const ELFFile<ELFT>& Obj, StringRef FileName,
                                                                  ELFObjectFileBase::elf_symbol_iterator_range Symbols)
{
  using Elf_Shdr = typename ELFT::Shdr;

  // We called sections() earlier, so can't fail here.
  typename ELFT::ShdrRange SectionsOrErr = cantFail(Obj.sections());
  const Elf_Shdr* SymVerSec              = nullptr;
  const Elf_Shdr* SymVerNeedSec          = nullptr;
  const Elf_Shdr* SymVerDefSec           = nullptr;
  for(const Elf_Shdr& Sec : SectionsOrErr)
  {
    if(Sec.sh_type == ELF::SHT_GNU_versym)
      SymVerSec = &Sec;
    else if(Sec.sh_type == ELF::SHT_GNU_verdef)
      SymVerDefSec = &Sec;
    else if(Sec.sh_type == ELF::SHT_GNU_verneed)
      SymVerNeedSec = &Sec;
  }

  if(!SymVerSec)
    return std::vector<SymbolVersion>{};

  Expected<SmallVector<Optional<VersionEntry>, 0>> MapOrErr = Obj.loadVersionMap(SymVerNeedSec, SymVerDefSec);
  if(!MapOrErr)
    return MapOrErr.takeError();

  std::vector<SymbolVersion> Ret;
  size_t I = 0;
  for(auto It = Symbols.begin(), E = Symbols.end(); It != E; ++It)
  {
    ++I;
    Expected<const typename ELFT::Versym*> VerEntryOrErr = Obj.template getEntry<typename ELFT::Versym>(*SymVerSec, I);
    if(!VerEntryOrErr)
      return createError("unable to read an entry with index " + Twine(I) + " from " + describe(Obj, *SymVerSec) + ": " +
                         toString(VerEntryOrErr.takeError()));

    Expected<uint32_t> FlagsOrErr = It->getFlags();
    if(!FlagsOrErr)
      return createError("unable to read flags for symbol with index " + Twine(I) + ": " +
                         toString(FlagsOrErr.takeError()));

    bool IsDefault;
    Expected<StringRef> VerOrErr = Obj.getSymbolVersionByIndex((*VerEntryOrErr)->vs_index, IsDefault, *MapOrErr,
                                                               (*FlagsOrErr) & SymbolRef::SF_Undefined);
    if(!VerOrErr)
      return createError("unable to get a version for entry " + Twine(I) + " of " + describe(Obj, *SymVerSec) + ": " +
                         toString(VerOrErr.takeError()));

    Ret.push_back({ (*VerOrErr).str(), IsDefault });
  }

  return Ret;
}


static Expected<std::vector<SymbolVersion>> readSymbolVersionsELF(const ELFObjectFileBase& Obj,
                                                                  ELFObjectFileBase::elf_symbol_iterator_range Symbols)
{
  if(const auto* ELF = dyn_cast<ELF32LEObjectFile>(&Obj))
    return readSymbolVersionsELF(ELF->getELFFile(), Obj.getFileName(), Symbols);
  else if(const auto* ELF = dyn_cast<ELF32BEObjectFile>(&Obj))
    return readSymbolVersionsELF(ELF->getELFFile(), Obj.getFileName(), Symbols);
  else if(const auto* ELF = dyn_cast<ELF64LEObjectFile>(&Obj))
    return readSymbolVersionsELF(ELF->getELFFile(), Obj.getFileName(), Symbols);
  return readSymbolVersionsELF(cast<ELF64BEObjectFile>(&Obj)->getELFFile(), Obj.getFileName(), Symbols);
}

static Error dumpSymbolNamesFromObject(std::vector<NMSymbol>& SymbolList, SymbolicFile& Obj, bool printName,
                                      StringRef ArchiveName = {}, StringRef ArchitectureName = {})
{
  auto Symbols = Obj.symbols();
  std::vector<SymbolVersion> SymbolVersions;
  /* if(DynamicSyms)
  {
    const auto* E = dyn_cast<ELFObjectFileBase>(&Obj);
    if(!E)
    {
      return createError("File format has no dynamic symbol table" + Obj.getFileName());
    }
    Symbols = E->getDynamicSymbolIterators();

    if(Expected<std::vector<SymbolVersion>> VersionsOrErr = readSymbolVersionsELF(*E, Symbols))
      SymbolVersions = std::move(*VersionsOrErr);
    else
      WithColor::warning(errs(), "innative")
        << "unable to read symbol versions: " << toString(VersionsOrErr.takeError()) << "\n";
  }*/

  // If a "-s segname sectname" option was specified and this is a Mach-O
  // file get the section number for that section in this object file.
  // MachOObjectFile* MachO = dyn_cast<MachOObjectFile>(&Obj);

  // if(!(MachO && DyldInfoOnly))
  {
    size_t I = -1;
    for(BasicSymbolRef Sym : Symbols)
    {
      ++I;
      Expected<uint32_t> SymFlagsOrErr = Sym.getFlags();
      if(!SymFlagsOrErr)
      {
        return SymFlagsOrErr.takeError();
      }

      // Don't drop format specifc symbols for ARM and AArch64 ELF targets, they
      // are used to repesent mapping symbols and needed to honor the
      // --special-syms option.
      auto* ELFObj = dyn_cast<ELFObjectFileBase>(&Obj);
      if((!ELFObj || (ELFObj->getEMachine() != ELF::EM_ARM && ELFObj->getEMachine() != ELF::EM_AARCH64)) &&
         (*SymFlagsOrErr & SymbolRef::SF_FormatSpecific))
        continue;

      NMSymbol S = {};
      S.Size     = 0;
      S.Address  = 0;
      if(isa<ELFObjectFileBase>(&Obj))
        S.Size = ELFSymbolRef(Sym).getSize();

      // if(const XCOFFObjectFile* XCOFFObj = dyn_cast<const XCOFFObjectFile>(&Obj))
      //   S.Size = XCOFFObj->getSymbolSize(Sym.getRawDataRefImpl());

      if(isa<ObjectFile>(Obj))
      {
        SymbolRef SymRef(Sym);
        Expected<uint64_t> AddressOrErr = SymRef.getAddress();
        if(!AddressOrErr)
        {
          consumeError(AddressOrErr.takeError());
          break;
        }
        S.Address = *AddressOrErr;
      }
      S.TypeName = getNMTypeName(Obj, Sym);
      S.TypeChar = getNMSectionTagAndName(Obj, Sym, S.SectionName);

      raw_string_ostream OS(S.Name);
      if(Error E = Sym.printName(OS))
      {
        return std::move(E);
      }
      if(!SymbolVersions.empty() && !SymbolVersions[I].Name.empty())
        S.Name += (SymbolVersions[I].IsDefault ? "@@" : "@") + SymbolVersions[I].Name;

      S.Sym = Sym;
      SymbolList.push_back(S);
    }
  }

  // sortAndPrintSymbolList(Obj, printName, ArchiveName, ArchitectureName);
  return Error::success();
}

Expected<std::vector<NMSymbol>> dumpSymbolNamesFromFile(const char* path, size_t size)
{
  std::unique_ptr<MemoryBuffer> mb;
  MemoryBufferRef mbref;
  if(!size)
  {
    auto MBOrErr = MemoryBuffer::getFileOrSTDIN(path);
    if(!MBOrErr)
    {
      return errorCodeToError(MBOrErr.getError());
    }
    mb    = std::move(*MBOrErr);
    mbref = *mb;
  }
  else
  {
    mbref = MemoryBufferRef(StringRef(path, size), "");
  }

  std::vector<NMSymbol> SymbolList;
  LLVMContext Context;
  LLVMContext* ContextPtr                       = &Context;
  Expected<std::unique_ptr<Binary>> BinaryOrErr = createBinary(mbref, ContextPtr);
  if(!BinaryOrErr)
  {
    return BinaryOrErr.takeError();
  }
  Binary& Bin = *BinaryOrErr.get();

  if(Archive* A = dyn_cast<Archive>(&Bin))
  {
    /*if(ArchiveMap)
    {
      Archive::symbol_iterator I = A->symbol_begin();
      Archive::symbol_iterator E = A->symbol_end();
      if(I != E)
      {
        outs() << "Archive map\n";
        for(; I != E; ++I)
        {
          Expected<Archive::Child> C = I->getMember();
          if(!C)
          {
            return C.takeError();
          }
          Expected<StringRef> FileNameOrErr = C->getName();
          if(!FileNameOrErr)
          {
            return FileNameOrErr.takeError();
          }
          StringRef SymName = I->getName();
          outs() << SymName << " in " << FileNameOrErr.get() << "\n";
        }
        outs() << "\n";
      }
    }*/

    {
      Error Err = Error::success();
      for(auto& C : A->children(Err))
      {
        Expected<std::unique_ptr<Binary>> ChildOrErr = C.getAsBinary(ContextPtr);
        if(!ChildOrErr)
        {
          if(auto E = isNotObjectErrorInvalidFileType(ChildOrErr.takeError()))
            return std::move(E);
          continue;
        }
        if(SymbolicFile* O = dyn_cast<SymbolicFile>(&*ChildOrErr.get()))
        {
          if(auto E = dumpSymbolNamesFromObject(SymbolList, *O, false, path))
            return std::move(E);
        }
      }
      if(Err)
        return std::move(Err);
    }
    return SymbolList;
  }

  if(TapiUniversal* TU = dyn_cast<TapiUniversal>(&Bin))
  {
    for(const TapiUniversal::ObjectForArch& I : TU->objects())
    {
      StringRef ArchName  = I.getArchFlagName();
      if(auto ObjOrErr = I.getAsObjectFile())
      {
        if(auto E = dumpSymbolNamesFromObject(SymbolList, *ObjOrErr.get(), false, {}, ArchName))
          return std::move(E);
      }
      else if(Error E = isNotObjectErrorInvalidFileType(ObjOrErr.takeError()))
      {
        return std::move(E);
      }
    }

    return SymbolList;
  }

  if(SymbolicFile* O = dyn_cast<SymbolicFile>(&Bin))
  {
    if(auto E =dumpSymbolNamesFromObject(SymbolList, *O, true))
      return std::move(E);
  }

  return SymbolList;
}
