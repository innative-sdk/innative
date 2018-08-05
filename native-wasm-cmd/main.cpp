// Copyright ©2018 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in native-wasm.h

#include "native-wasm/export.h"
#include <iostream>

int main(int argc, char *argv[])
{
  //int r = innative_compile_script("../spec/test/core/align.wast", ENV_DEBUG | ENV_STRICT);
  int r = innative_compile_file("test.wast", "out.exe", ENV_DEBUG|ENV_STRICT|ENV_ENABLE_WAT, false, 0, 0);
  char c;
  std::cin >> c;
  return r;
}