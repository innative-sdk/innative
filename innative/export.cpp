// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/export.h"
#include "tools.h"
#include "utility.h"

using namespace innative;

// Return pointers to all our internal functions
void innative_runtime(INExports* exports)
{
  exports->CreateEnvironment       = &CreateEnvironment;
  exports->AddModule               = &AddModule;
  exports->AddModuleObject         = &AddModuleObject;
  exports->AddWhitelist            = &AddWhitelist;
  exports->AddEmbedding            = &AddEmbedding;
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
  exports->ReplaceTableFuncPtr     = &ReplaceTableFuncPtr;
  exports->LoadAssembly            = &LoadAssembly;
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
}

void innative_set_work_dir_to_bin(const char* arg0)
{
  utility::SetWorkingDir(utility::GetProgramPath(arg0).parent_path().u8string().c_str());
}

int innative_install(const char* arg0, bool full) { return utility::Install(arg0, full); }

int innative_uninstall() { return utility::Uninstall(); }
