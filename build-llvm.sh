#!/bin/sh
git submodule update --init --recursive

mkdir -p bin/llvm
cd bin/llvm

if [ $# -gt 0 ] && [ "$1" = "ninja" ]; then
  echo "Using Ninja"
  cmake -GNinja ../../llvm -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE:STRING="MinSizeRel" -DLLVM_TARGETS_TO_BUILD:STRING="X86;WebAssembly" -DLLVM_BUILD_LLVM_DYLIB:BOOL=OFF -DLLVM_OPTIMIZED_TABLEGEN:BOOL=ON -DLLVM_INCLUDE_EXAMPLES:BOOL=OFF -DLLVM_INCLUDE_TESTS:BOOL=OFF -DLLVM_INCLUDE_BENCHMARKS:BOOL=OFF -DLLVM_APPEND_VC_REV:BOOL=OFF
  ninja
else
  CORES=$(nproc --all)
  if [ -z "$CORES" ]; then
    CORES=1
  fi
  echo "Using Make with" $CORES "cores"
  cmake ../../llvm -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE:STRING="MinSizeRel" -DLLVM_TARGETS_TO_BUILD:STRING="X86;WebAssembly" -DLLVM_BUILD_LLVM_DYLIB:BOOL=OFF -DLLVM_OPTIMIZED_TABLEGEN:BOOL=ON -DLLVM_INCLUDE_EXAMPLES:BOOL=OFF -DLLVM_INCLUDE_TESTS:BOOL=OFF -DLLVM_INCLUDE_BENCHMARKS:BOOL=OFF -DLLVM_APPEND_VC_REV:BOOL=OFF
  make -j $CORES
fi
cd ../..

echo "Building LLD"
mkdir -p bin/lld
cd bin/lld
cmake -GNinja ../../llvm-project/lld -DLLVM_TARGETS_TO_BUILD="X86;WebAssembly" -DLLVM_CONFIG_PATH="../llvm/bin/llvm-config" -DLLVM_TABLEGEN_EXE="../llvm/bin/llvm-tblgen" -DLLVM_INCLUDE_TESTS=OFF -DCMAKE_CXX_FLAGS_RELEASE="/MT" -DCMAKE_CXX_FLAGS_MINSIZEREL="/MT" -DCMAKE_CXX_FLAGS_RELWITHDEBINFO="/MT" -DCMAKE_CXX_FLAGS_DEBUG="/MTd" 
ninja
cd ../..
cp llvm-project/lld/include/* bin/lld/include -R
cp bin/lld/lib/*.a bin/llvm/lib/*.a
