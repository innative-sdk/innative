Function Find-MsBuild([int] $MaxVersion = 2017)
{
    $agentPath = "$Env:programfiles (x86)\Microsoft Visual Studio\2017\BuildTools\MSBuild\15.0\Bin\msbuild.exe"
    $devPath = "$Env:programfiles (x86)\Microsoft Visual Studio\2017\Enterprise\MSBuild\15.0\Bin\msbuild.exe"
    $proPath = "$Env:programfiles (x86)\Microsoft Visual Studio\2017\Professional\MSBuild\15.0\Bin\msbuild.exe"
    $communityPath = "$Env:programfiles (x86)\Microsoft Visual Studio\2017\Community\MSBuild\15.0\Bin\msbuild.exe"
    $fallback2015Path = "${Env:ProgramFiles(x86)}\MSBuild\14.0\Bin\MSBuild.exe"
    $fallback2013Path = "${Env:ProgramFiles(x86)}\MSBuild\12.0\Bin\MSBuild.exe"
    $fallbackPath = "C:\Windows\Microsoft.NET\Framework\v4.0.30319"
		
    If ((2017 -le $MaxVersion) -And (Test-Path $agentPath)) { return $agentPath } 
    If ((2017 -le $MaxVersion) -And (Test-Path $devPath)) { return $devPath } 
    If ((2017 -le $MaxVersion) -And (Test-Path $proPath)) { return $proPath } 
    If ((2017 -le $MaxVersion) -And (Test-Path $communityPath)) { return $communityPath } 
    If ((2015 -le $MaxVersion) -And (Test-Path $fallback2015Path)) { return $fallback2015Path } 
    If ((2013 -le $MaxVersion) -And (Test-Path $fallback2013Path)) { return $fallback2013Path } 
    If (Test-Path $fallbackPath) { return $fallbackPath } 
        
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
if(!(Test-Path "./cmake-3.11.4-win32-x86/bin/cmake.exe" -PathType Leaf))
{
  Start-BitsTransfer -DisplayName CMake -Source "https://cmake.org/files/v3.11/cmake-3.11.4-win32-x86.zip" -Destination .
  Expand-Archive "./cmake-3.11.4-win32-x86.zip" -DestinationPath .
}

# If we can't find Python, download it
if(!(Test-Path "./python3/python.exe" -PathType Leaf))
{
  Start-BitsTransfer -DisplayName Python3 -Source "https://www.python.org/ftp/python/3.5.4/python-3.5.4-embed-win32.zip" -Destination .
  Expand-Archive "./python-3.5.4-embed-win32.zip" -DestinationPath ./python3
}

cd llvm

..\cmake-3.11.4-win32-x86\bin\cmake.exe -G "Visual Studio 15 Win64" -DLLVM_TARGETS_TO_BUILD="X86;WebAssembly" -DLLVM_INCLUDE_EXAMPLES=OFF -DLLVM_INCLUDE_TESTS=OFF -DLLVM_INCLUDE_UTILS=OFF -DLLVM_BUILD_TOOLS=ON -DCMAKE_CXX_FLAGS_RELEASE="/MT" -DCMAKE_CXX_FLAGS_MINSIZEREL="/MT" -DCMAKE_CXX_FLAGS_RELWITHDEBINFO="/MT" -DCMAKE_CXX_FLAGS_DEBUG="/MTd" -D PYTHON_EXECUTABLE="../python3/python.exe" ../../llvm

$params = 'llvm.sln', '/p:Configuration=MinSizeRel'
& $msbuild $params
$params = 'llvm.sln', '/p:Configuration=Debug'
& $msbuild $params

echo "Finished building LLVM!"

cd ..
cd ..