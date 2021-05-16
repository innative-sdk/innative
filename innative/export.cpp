// Copyright (c)2021 Fundament Software
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/export.h"
#include "tools.h"
#include "utility.h"

using namespace innative;

// Return pointers to all our internal functions
void innative_runtime(INExports* exports)
{
  exports->CreateEnvironment       = &CreateEnvironment;
  exports->GetDefaultEmbedding     = &GetDefaultEmbedding;
  exports->GetEmbeddingPath        = &GetEmbeddingPath;
  exports->AddModule               = &AddModule;
  exports->AddModuleObject         = &AddModuleObject;
  exports->AddWhitelist            = &AddWhitelist;
  exports->AddEmbedding            = &AddEmbedding;
  exports->AddCPUFeature           = &AddCPUFeature;
  exports->AddCustomExport         = &AddCustomExport;
  exports->FinalizeEnvironment     = &FinalizeEnvironment;
  exports->Validate                = &Validate;
  exports->Compile                 = &Compile;
  exports->LoadFunction            = &LoadFunction;
  exports->LoadTable               = &LoadTable;
  exports->LoadGlobal              = &LoadGlobal;
  exports->GetModuleMetadata       = &GetModuleMetadata;
  exports->LoadMemoryIndex         = &LoadMemoryIndex;
  exports->LoadTableIndex          = &LoadTableIndex;
  exports->LoadGlobalIndex         = &LoadGlobalIndex;
  exports->DumpJITState            = &DumpJITState;
  exports->CompileJIT              = &CompileJIT;
  exports->LoadFunctionJIT         = &LoadFunctionJIT;
  exports->LoadTableJIT            = &LoadTableJIT;
  exports->LoadGlobalJIT           = &LoadGlobalJIT;
  exports->GetModuleMetadataJIT    = &GetModuleMetadataJIT;
  exports->LoadMemoryIndexJIT      = &LoadMemoryIndexJIT;
  exports->LoadTableIndexJIT       = &LoadTableIndexJIT;
  exports->LoadGlobalIndexJIT      = &LoadGlobalIndexJIT;
  exports->ReplaceTableFuncPtr     = &ReplaceTableFuncPtr;
  exports->LoadAssembly            = &LoadAssembly;
  exports->LoadAssemblyError       = &utility::LoadDLLError;
  exports->LoadAssemblyErrorFree   = &utility::LoadDLLErrorFree;
  exports->FreeAssembly            = &FreeAssembly;
  exports->ClearEnvironmentCache   = &ClearEnvironmentCache;
  exports->GetTypeEncodingString   = &GetTypeEncodingString;
  exports->GetErrorString          = &GetErrorString;
  exports->DestroyEnvironment      = &DestroyEnvironment;
  exports->CompileScript           = &CompileScript;
  exports->SerializeModule         = &SerializeModule;
  exports->LoadSourceMap           = &LoadSourceMap;
  exports->SerializeSourceMap      = &SerializeSourceMap;
  exports->InsertModuleSection     = &InsertModuleSection;
  exports->DeleteModuleSection     = &DeleteModuleSection;
  exports->SetByteArray            = &SetByteArray;
  exports->SetIdentifier           = &SetIdentifier;
  exports->InsertModuleLocal       = &InsertModuleLocal;
  exports->RemoveModuleLocal       = &RemoveModuleLocal;
  exports->InsertModuleInstruction = &InsertModuleInstruction;
  exports->RemoveModuleInstruction = &RemoveModuleInstruction;
  exports->InsertModuleParam       = &InsertModuleParam;
  exports->RemoveModuleParam       = &RemoveModuleParam;
  exports->InsertModuleReturn      = &InsertModuleReturn;
  exports->RemoveModuleReturn      = &RemoveModuleReturn;

#ifdef IN_DEBUG
  // Because this is just a set of pointers, we can iterate through them all to check for NULL pointers
  for(size_t i = 0; i < sizeof(INExports) / sizeof(void*); ++i)
    assert(reinterpret_cast<void**>(exports)[i] != 0);
#endif
}

void innative_set_work_dir_to_bin(const char* arg0)
{
  utility::SetWorkingDir(utility::GetProgramPath(arg0).parent_path().u8string().c_str());
}

int innative_install(const char* arg0, bool full) { return utility::Install(arg0, full); }

int innative_uninstall() { return utility::Uninstall(); }
