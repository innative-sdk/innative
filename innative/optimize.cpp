// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#pragma warning(push)
#pragma warning(disable:4146)
#define _SCL_SECURE_NO_WARNINGS
#include "optimize.h"
#pragma warning(pop)

using namespace innative;

// Generate function annotations based on dependency graph.
IR_ERROR innative::AnnotateFunctions(Environment* env, code::Context* contexts)
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