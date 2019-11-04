#!/bin/sh
git submodule update --init --recursive

mkdir -p bin/lld
cd bin/lld

export BIN_DIR=$(readlink --canonicalize ../llvm/bin)

if [ $# -gt 0 ] && [ "$1" = "ninja" ]; then
  echo "Using Ninja"
  cmake -GNinja ../../llvm-project/lld -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=ON -DCMAKE_BUILD_TYPE:STRING="MinSizeRel" -DLLVM_TARGETS_TO_BUILD:STRING="X86;WebAssembly" -DLLVM_INCLUDE_TESTS:BOOL=OFF -DLLVM_CONFIG_PATH:STRING="$BIN_DIR/llvm-config" -DLLVM_TABLEGEN_EXE:STRING="$BIN_DIR/llvm-tblgen"
  ninja
else
  CORES=$(nproc --all)
  if [ -z "$CORES" ]; then
    CORES=1
  fi
  echo "Using Make with" $CORES "cores"
  cmake ../../llvm-project/lld -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=ON -DCMAKE_BUILD_TYPE:STRING="MinSizeRel" -DLLVM_TARGETS_TO_BUILD:STRING="X86;WebAssembly" -DLLVM_INCLUDE_TESTS:BOOL=OFF -DLLVM_CONFIG_PATH:STRING="$BIN_DIR/llvm-config" -DLLVM_TABLEGEN_EXE:STRING="$BIN_DIR/llvm-tblgen"
  make -j $CORES
fi
cd ../..

cp bin/lld/lib/*.a bin/llvm/lib -f

cd llvm-project/lld/include/
find ./ -type f \( -iname \*.h -o -iname \*.inc -o -iname \*.def -o -iname \*.td -o -iname \*.modulemap \) -exec cp --parents {} ../../../bin/llvm/include/ -n \;
cd ../../..
