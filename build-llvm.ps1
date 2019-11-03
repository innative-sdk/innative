# Update all submodules
git submodule update --init --recursive

# make directories
if(!(Test-Path -path "./bin/llvm"))
{
  mkdir -p bin/llvm
}

invoke-expression -Command .\get-cmake.ps1

pushd bin
pushd llvm

..\cmake\bin\cmake.exe "-Tv142,host=x64" -DCMAKE_GENERATOR_PLATFORM="x64" -DLLVM_TARGETS_TO_BUILD="X86;WebAssembly" -DLLVM_OPTIMIZED_TABLEGEN=ON -DLLVM_ENABLE_TERMINFO=OFF -DLLVM_ENABLE_ZLIB=OFF -DLLVM_INCLUDE_DOCS=OFF -DLLVM_INCLUDE_EXAMPLES=OFF -DLLVM_INCLUDE_TESTS=OFF -DLLVM_INCLUDE_TOOLS=ON -DLLVM_INCLUDE_UTILS=OFF -DCMAKE_CXX_FLAGS_RELEASE="/MT" -DCMAKE_CXX_FLAGS_MINSIZEREL="/MT" -DCMAKE_CXX_FLAGS_RELWITHDEBINFO="/MT" -DCMAKE_CXX_FLAGS_DEBUG="/MTd" ../../llvm-project/llvm
 
..\cmake\bin\cmake.exe --build . --config MinSizeRel
..\cmake\bin\cmake.exe --build . --config Debug

if(!(Test-Path -path "./bin/"))
{
  mkdir -p bin
}

Copy-Item -Path "MinSizeRel\bin\*" -Destination "bin" -Force
Copy-Item -Path "MinSizeRel\lib\*.lib" -Destination "lib" -Force

popd
popd

robocopy "llvm-project\llvm\include" "bin\llvm\include" *.h *.inc *.def *.td *.modulemap /S /XO /XN /XC

echo "Finished building LLVM!"