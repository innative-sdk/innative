$cmakever = "3.15.4"

# If we can't find CMake, download it
if(!(Test-Path "./cmake/bin/cmake.exe" -PathType Leaf))
{
  $cmakeshort = $cmakever.substring(0,4)
  Start-BitsTransfer -DisplayName CMake -Source "https://cmake.org/files/v$cmakeshort/cmake-$cmakever-win32-x86.zip" -Destination .
  Expand-Archive "./cmake-$cmakever-win32-x86.zip" -DestinationPath .
  Rename-Item -Path "cmake-$cmakever-win32-x86" -NewName "cmake"
}