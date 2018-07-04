// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#include "native-wasm/export.h"
#include <iostream>

int main(int argc, char *argv[])
{
  int r = native_wasm_compile_file("test.wasm", "out.exe", ENV_DEBUG|ENV_STRICT, false, 0, 0);
  char c;
  std::cin >> c;
  return r;
}