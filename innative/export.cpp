// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/export.h"
#include "tools.h"
#include "util.h"

using namespace innative;

// Return pointers to all our internal functions
void innative_runtime(IRExports* exports)
{
  exports->CreateEnvironment     = &CreateEnvironment;
  exports->AddModule             = &AddModule;
  exports->AddWhitelist          = &AddWhitelist;
  exports->AddEmbedding          = &AddEmbedding;
  exports->FinalizeEnvironment   = &FinalizeEnvironment;
  exports->Validate              = &Validate;
  exports->Compile               = &Compile;
  exports->LoadFunction          = &LoadFunction;
  exports->LoadTable             = &LoadTable;
  exports->LoadGlobal            = &LoadGlobal;
  exports->LoadAssembly          = &LoadAssembly;
  exports->FreeAssembly          = &FreeAssembly;
  exports->ClearEnvironmentCache = &ClearEnvironmentCache;
  exports->GetTypeEncodingString = &GetTypeEncodingString;
  exports->GetErrorString        = &GetErrorString;
  exports->CompileScript         = &CompileScript;
  exports->SerializeModule       = &SerializeModule;
  exports->DestroyEnvironment    = &DestroyEnvironment;
}

void innative_set_work_dir_to_bin(const char* arg0)
{
  utility::SetWorkingDir(utility::GetProgramPath(arg0).parent_path().u8string().c_str());
}

int innative_install(const char* arg0, bool full) { return utility::Install(arg0, full); }

int innative_uninstall() { return utility::Uninstall(); }
