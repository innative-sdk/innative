param([Int32]$version=2019)

Function Find-MsBuild()
{
    $communityPath2019 = "$Env:programfiles (x86)\Microsoft Visual Studio\2017\Community\MSBuild\15.0\Bin\msbuild.exe"
    $fallback2017Path = "${Env:ProgramFiles(x86)}\MSBuild\15.0\Bin\MSBuild.exe"
    $fallback2015Path = "${Env:ProgramFiles(x86)}\MSBuild\14.0\Bin\MSBuild.exe"
    $fallback2013Path = "${Env:ProgramFiles(x86)}\MSBuild\12.0\Bin\MSBuild.exe"
    $fallbackPath4 = "C:\Windows\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe"
    $fallbackPath3 = "C:\Windows\Microsoft.NET\Framework\v3.0\MSBuild.exe"
		
    If (Test-Path $communityPath2019) { return $communityPath } 
    If (Test-Path $fallback2017Path) { return $fallback2017Path } 
    If ((Test-Path $fallback2015Path) { return $fallback2015Path } 
    If (Test-Path $fallback2013Path) { return $fallback2013Path } 
    If (Test-Path $fallbackPath4) { return $fallbackPath4 } 
    If (Test-Path $fallbackPath3) { return $fallbackPath3 } 
        
    throw "Yikes - Unable to find msbuild"
}

$msbuild = Find-MsBuild

# Update all submodules
#git submodule update --init --recursive

# make directories
if(!(Test-Path -path "./bin/llvm"))
{
  mkdir -p bin/llvm
}

cd bin

# If we can't find CMake, download it
if(!(Test-Path "./cmake-3.14.1-win32-x86/bin/cmake.exe" -PathType Leaf))
{
  Start-BitsTransfer -DisplayName CMake -Source "https://cmake.org/files/v3.14/cmake-3.14.1-win32-x86.zip" -Destination .
  Expand-Archive "./cmake-3.14.1-win32-x86.zip" -DestinationPath .
}

# If we can't find Python, download it
if(!(Test-Path "./python3/python.exe" -PathType Leaf))
{
  Start-BitsTransfer -DisplayName Python3 -Source "https://www.python.org/ftp/python/3.5.4/python-3.5.4-embed-win32.zip" -Destination .
  Expand-Archive "./python-3.5.4-embed-win32.zip" -DestinationPath ./python3
}

cd llvm

if($version -eq 2017)
{
  ..\cmake-3.14.1-win32-x86\bin\cmake.exe -G "Visual Studio 15 Win64" -DLLVM_TARGETS_TO_BUILD="X86;WebAssembly" -DLLVM_INCLUDE_EXAMPLES=OFF -DLLVM_INCLUDE_TESTS=OFF -DLLVM_INCLUDE_UTILS=OFF -DLLVM_BUILD_TOOLS=ON -DCMAKE_CXX_FLAGS_RELEASE="/MT" -DCMAKE_CXX_FLAGS_MINSIZEREL="/MT" -DCMAKE_CXX_FLAGS_RELWITHDEBINFO="/MT" -DCMAKE_CXX_FLAGS_DEBUG="/MTd" -D PYTHON_EXECUTABLE="../python3/python.exe" ../../llvm
}
elseif($version -eq 2019)
{
  ..\cmake-3.14.1-win32-x86\bin\cmake.exe -G "Visual Studio 16" "-Tv141_xp,host=x64" -DCMAKE_GENERATOR_PLATFORM="x64" -DLLVM_TARGETS_TO_BUILD="X86;WebAssembly" -DLLVM_INCLUDE_EXAMPLES=OFF -DLLVM_INCLUDE_TESTS=OFF -DLLVM_INCLUDE_UTILS=OFF -DLLVM_BUILD_TOOLS=ON -DCMAKE_CXX_FLAGS_RELEASE="/MT" -DCMAKE_CXX_FLAGS_MINSIZEREL="/MT" -DCMAKE_CXX_FLAGS_RELWITHDEBINFO="/MT" -DCMAKE_CXX_FLAGS_DEBUG="/MTd" -D PYTHON_EXECUTABLE="../python3/python.exe" ../../llvm
  
  echo "Visual Studio 2019 broke MSBuild, but the solution still works inside Visual Studio. Open bin/llvm/LLVM.sln and manually build Debug and MinSizeRel."
  return
}
else
{
  echo "Unrecognized version! Only Visual Studio 2017 and Visual Studio 2019 are supported."
  return
}

$params = 'llvm.sln', '/p:Configuration=MinSizeRel'
& $msbuild $params
$params = 'llvm.sln', '/p:Configuration=Debug'
& $msbuild $params

echo "Finished building LLVM!"

cd ..
cd ..