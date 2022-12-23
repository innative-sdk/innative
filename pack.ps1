param([string]$arch, [switch]$runtime)

function Packer {
  param (
      [string]$arch,
      [switch]$runtime
  )
  
  switch ($arch) {
    "x86-64" {$libdir = "windows-amd64"}
    "x64" {$libdir = "windows-amd64"}
    "x86" {$libdir = "windows-i386"}
    "arm" {$libdir = "windows-aarch64"}
  }
  $mode = "sdk"
  if ($runtime) {
    $mode = "runtime"
  }
  $target = "innative-windows-$mode-$arch"

  if ($runtime) {
  New-Item -Force "$target\$libdir\" -Type Directory
  Copy-Item "bin\innative-test.exe" -Destination "$target\"
  Copy-Item "bin\innative-cmd.exe" -Destination "$target\"
  Copy-Item "bin\innative-cmd-d.exe" -Destination "$target\"
  Copy-Item "bin\innative.dll" -Destination "$target\"
  Copy-Item "bin\innative-d.dll" -Destination "$target\"
  Copy-Item "bin\innative-s.lib" -Destination "$target\"
  Copy-Item "bin\innative-s-d.lib" -Destination "$target\"
  Copy-Item "bin\$libdir\innative-env.lib" -Destination "$target\$libdir"
  Copy-Item "bin\$libdir\innative-env-d.lib" -Destination "$target\$libdir"
  Copy-Item "bin\$libdir\innative-assemblyscript.lib" -Destination "$target\$libdir"
  Copy-Item "bin\$libdir\innative-assemblyscript-d.lib" -Destination "$target\$libdir"
  } else {
  New-Item -Force "$target\bin\$libdir\" -Type Directory
  New-Item -Force "$target\scripts\" -Type Directory
  New-Item -Force "$target\include\innative\" -Type Directory
  New-Item -Force "$target\spec\test\core\" -Type Directory

  $bins = @("innative", "innative-d", "innative-s", "innative-s-d", "innative-stub", "innative-stub-d")
  $libs = @("innative-assemblyscript", "innative-assemblyscript-d", "innative-env", "innative-env-d", "innative-test-embedding", "innative-test-embedding-d")
  foreach ($element in $bins) {
  Copy-Item "bin\$element.dll" -Destination "$target\bin\" -ErrorAction SilentlyContinue
  Copy-Item "bin\$element.exp" -Destination "$target\bin\" -ErrorAction SilentlyContinue
  Copy-Item "bin\$element.lib" -Destination "$target\bin\"
  Copy-Item "bin\$element.pdb" -Destination "$target\bin\" -ErrorAction SilentlyContinue
  }
  foreach ($element in $libs) {
  Copy-Item "bin\$libdir\$element.dll" -Destination "$target\bin\$libdir\" -ErrorAction SilentlyContinue
  Copy-Item "bin\$libdir\$element.exp" -Destination "$target\bin\$libdir\" -ErrorAction SilentlyContinue
  Copy-Item "bin\$libdir\$element.lib" -Destination "$target\bin\$libdir\"
  Copy-Item "bin\$libdir\$element.pdb" -Destination "$target\bin\$libdir\" -ErrorAction SilentlyContinue
  }

  Copy-Item "bin\innative-test.exe" -Destination "$target\bin\"
  Copy-Item "bin\innative-cmd.exe" -Destination "$target\bin\"
  Copy-Item "bin\innative-cmd-d.exe" -Destination "$target\bin\"
  Copy-Item "bin\innative-loader.exe" -Destination "$target\bin\"
  Copy-Item "bin\innative-loader-d.exe" -Destination "$target\bin\"
  Copy-Item "include\innative\*" -Destination "$target\include\innative\"
  Copy-Item "scripts\*.wat" -Destination "$target\scripts\" 
  Copy-Item "scripts\*.wasm" -Destination "$target\scripts\" 
  Copy-Item "spec\test\core\*.wast" -Destination "$target\spec\test\core\" 
  }

  Compress-Archive -Force -Path "$target" -CompressionLevel Optimal -DestinationPath "$target.zip"
  Remove-Item "$target" -Recurse
}

if([string]::IsNullOrWhitespace($arch)) {
  Packer -arch "x64"
  Packer -arch "x64" -runtime
  Packer -arch "x86"
  Packer -arch "x86" -runtime
} else {
  if($runtime) { 
    Packer -arch $arch -runtime
  } else {
    Packer -arch $arch
  }
}