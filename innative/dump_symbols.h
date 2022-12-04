// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__DUMP_SYMBOLS_H
#define IN__DUMP_SYMBOLS_H

#include "llvm.h"

struct NMSymbol
{
  uint64_t Address;
  uint64_t Size;
  char TypeChar;
  std::string Name;
  llvm::StringRef SectionName;
  llvm::StringRef TypeName;
  llvm::object::BasicSymbolRef Sym;
  llvm::StringRef Visibility;

  // The Sym field above points to the native symbol in the object file,
  // for Mach-O when we are creating symbols from the dyld info the above
  // pointer is null as there is no native symbol.  In these cases the fields
  // below are filled in to represent what would have been a Mach-O nlist
  // native symbol.
  uint32_t SymFlags;
  llvm::object::SectionRef Section;
  uint8_t NType;
  uint8_t NSect;
  uint16_t NDesc;
  std::string IndirectName;
};

llvm::Expected<std::vector<NMSymbol>> dumpSymbolNamesFromFile(const char* path, size_t size);

#endif