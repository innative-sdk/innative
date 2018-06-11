// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#pragma warning(push)
#pragma warning(disable:4146)
#define _SCL_SECURE_NO_WARNINGS
#include "optimize.h"
#pragma warning(pop)

// Wrap all external functions in a fastcc inline'd internal function. This allows LLVM to use
// fastcc calling convention on all direct function calls, but any external calls that are used
// in an indirect call will be properly wrapped to use a C calling convention
ERROR_CODE WrapFunctions(Environment* env, NWContext& context)
{
  // First, determine any modules that are imported but aren't included in the compilation
  // This indicates that the module must be external, so all these functions must use C calls

  // Then, for all functions using the C calling function, wrap them with an internal fastcc call
  // Ensure this is inlined whenever possible
  // This internal function replaces our function reference
  return ERR_SUCCESS;
}

// Generate function annotations based on dependency graph.
ERROR_CODE AnnotateFunctions(Environment* env, NWContext* contexts)
{
  // Go through every single function and start by considering it maximally pure, then remove 
  // tags as we walk through the code, ignoring function calls. If it has no function body,
  // assume the worst case.

  // Generate a DAG of all function calls in all modules. The root should be either the start
  // function, or all exported functions, and the leaves should be unresolved external dependencies.

  // Starting at the leaves, walk up each layer of the graph, removing any tag from a function that
  // calls another function with an incompatible tag, and marking each visited function to avoid cycles.

  return ERR_SUCCESS;
}