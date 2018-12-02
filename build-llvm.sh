#!/bin/bash
git submodule update --init --recursive

mkdir -p bin/llvm
cd bin/llvm

cmake ../../llvm -DLLVM_TARGETS_TO_BUILD:STRING="X86" -DLLVM_BUILD_LLVM_DYLIB:BOOL=ON -DLLVM_OPTIMIZED_TABLEGEN:BOOL=ON
make

cd lib
g++ -Wl,-whole-archive liblldCOFF.a liblldCommon.a liblldCore.a liblldDriver.a liblldELF.a liblldMachO.a liblldMinGW.a liblldReaderWriter.a liblldWasm.a liblldYAML.a -shared -o liblld-8svn.so -Wl,-no-whole-archive -L. -lpthread -lLLVM-8svn 

cp libLLVM-8svn.so ../../
cp liblld-8svn.so ../../
