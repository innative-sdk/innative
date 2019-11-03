# Update all submodules
git submodule update --init --recursive

# make directories
if(!(Test-Path -path "./bin/lld"))
{
  mkdir -p bin/lld
}

invoke-expression -Command .\get-cmake.ps1

pushd bin
pushd lld

..\cmake\bin\cmake.exe "-Tv142,host=x64" -DCMAKE_GENERATOR_PLATFORM="x64" -DLLVM_TARGETS_TO_BUILD="X86;WebAssembly" -DLLVM_CONFIG_PATH="../llvm/bin/llvm-config.exe" -DLLVM_TABLEGEN_EXE="../llvm/bin/llvm-tblgen.exe" -DLLVM_INCLUDE_TESTS=OFF -DCMAKE_CXX_FLAGS_RELEASE="/MT" -DCMAKE_CXX_FLAGS_MINSIZEREL="/MT" -DCMAKE_CXX_FLAGS_RELWITHDEBINFO="/MT" -DCMAKE_CXX_FLAGS_DEBUG="/MTd" ../../llvm-project/lld
 
..\cmake\bin\cmake.exe --build . --config MinSizeRel
..\cmake\bin\cmake.exe --build . --config Debug

popd
popd

robocopy "llvm-project\lld\include" "bin\lld\include" *.h *.inc *.def *.td *.modulemap /S /XO /XN /XC

echo "Finished building LLD!"