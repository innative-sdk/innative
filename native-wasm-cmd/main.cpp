// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#include "native-wasm/export.h"
#include <iostream>

int main(int argc, char *argv[])
{
  int r = native_wasm_compile_file(argv[1], "out.exe", 0, false);
  char c;
  std::cin >> c;
  return r;
}