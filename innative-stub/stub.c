// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "innative/export.h"

// This is a stub loader for the runtime. It looks for an existing installation
// of the runtime on the OS that is equal to or newer than the compiled version.
IR_COMPILER_DLLEXPORT extern void innative_runtime(IRExports* exports)
{
#ifdef IR_PLATFORM_WIN32
  // On windows, we use the registry to store versions, with a key set to the DLL path of the runtime.
  // We prefer using an exact match to our compiled version if it is available. Otherwise, we get the next closest version.

#elif defined(IR_PLATFORM_POSIX)
  // On linux, we use symlinks, but still prefer getting an exact version match if possible
#error TODO
  // List of paths to check
#else
#error Unknown platform!
#endif
}