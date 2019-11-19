#!/bin/sh
git submodule update --init --recursive

mkdir -p bin/llvm
cd bin/llvm

if [ $# -gt 0 ] && [ "$1" = "ninja" ]; then
  echo "Using Ninja"
  cmake -GNinja ../../llvm-project/llvm -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE:STRING="MinSizeRel" -DLLVM_TARGETS_TO_BUILD:STRING="X86;WebAssembly" -DLLVM_BUILD_LLVM_DYLIB:BOOL=OFF -DLLVM_ENABLE_TERMINFO:BOOL=OFF -DLLVM_INCLUDE_DOCS:BOOL=OFF -DLLVM_OPTIMIZED_TABLEGEN:BOOL=ON -DLLVM_INCLUDE_EXAMPLES:BOOL=OFF -DLLVM_INCLUDE_TESTS:BOOL=OFF -DLLVM_INCLUDE_BENCHMARKS:BOOL=OFF -DLLVM_ENABLE_ZLIB:BOOL=OFF -DLLVM_APPEND_VC_REV:BOOL=OFF
  if [ $? -eq 0 ]
  then
      ninja
  else
      echo "CMake failed, please check whether CMake is available in your system"
      exit -1
  fi
else
  CORES=$(nproc --all)
  if [ -z "$CORES" ]; then
    CORES=1
  fi
  echo "Using Make with" $CORES "cores"
  cmake ../../llvm-project/llvm -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE:STRING="MinSizeRel" -DLLVM_TARGETS_TO_BUILD:STRING="X86;WebAssembly" -DLLVM_BUILD_LLVM_DYLIB:BOOL=OFF -DLLVM_ENABLE_TERMINFO:BOOL=OFF -DLLVM_INCLUDE_DOCS:BOOL=OFF -DLLVM_OPTIMIZED_TABLEGEN:BOOL=ON -DLLVM_INCLUDE_EXAMPLES:BOOL=OFF -DLLVM_INCLUDE_TESTS:BOOL=OFF -DLLVM_INCLUDE_BENCHMARKS:BOOL=OFF -DLLVM_ENABLE_ZLIB:BOOL=OFF -DLLVM_APPEND_VC_REV:BOOL=OFF
  if [ $? -eq 0 ]
  then
      make -j $CORES
  else
      echo "CMake failed, please check whether CMake is available in your system"
      exit -1
  fi
fi
cd ../..

cd llvm-project/llvm/include/
find ./ -type f \( -iname \*.h -o -iname \*.inc -o -iname \*.def -o -iname \*.td -o -iname \*.modulemap \) -exec cp --parents {} ../../../bin/llvm/include/ -n \;
cd ../../..
