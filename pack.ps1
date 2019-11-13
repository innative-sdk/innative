param([string]$arch="x64", [switch]$runtime)

switch ($arch) {
  "x86-64" {$bin = "bin"}
  "x64" {$bin = "bin"}
  "x86" {$bin = "bin-Win32"}
  "arm" {$bin = "bin-ARM"}
}
$mode = "sdk"
if ($runtime) {
  $mode = "runtime"
}
$target = "innative-windows-$mode-$arch"

if ($runtime) {
Compress-Archive -Force -Path "$bin\innative.dll","$bin\innative-d.dll","$bin\innative-s.lib","$bin\innative-s-d.lib","$bin\innative-env.lib","$bin\innative-env-d.lib","$bin\innative-cmd.exe","$bin\innative-cmd-d.exe" -CompressionLevel Optimal -DestinationPath "$target.zip"
} else {
New-Item -Force "$target\bin\" -Type Directory
New-Item -Force "$target\scripts\" -Type Directory
New-Item -Force "$target\include\innative\" -Type Directory
New-Item -Force "$target\spec\test\core\" -Type Directory

$libs = @("innative", "innative-d", "innative-s", "innative-s-d", "innative-stub", "innative-stub-d", "innative-env", "innative-env-d", "innative-test-embedding", "innative-test-embedding-d")
foreach ($element in $libs) {
Copy-Item "$bin\$element.dll" -Destination "$target\bin\" -ErrorAction SilentlyContinue
Copy-Item "$bin\$element.exp" -Destination "$target\bin\" -ErrorAction SilentlyContinue
Copy-Item "$bin\$element.lib" -Destination "$target\bin\"
Copy-Item "$bin\$element.pdb" -Destination "$target\bin\" -ErrorAction SilentlyContinue
}

Copy-Item "$bin\innative-test.exe" -Destination "$target\bin\"
Copy-Item "$bin\innative-cmd.exe" -Destination "$target\bin\"
Copy-Item "$bin\innative-cmd-d.exe" -Destination "$target\bin\"
Copy-Item "$bin\innative-loader.exe" -Destination "$target\bin\"
Copy-Item "$bin\innative-loader-d.exe" -Destination "$target\bin\"
Copy-Item "include\innative\*" -Destination "$target\include\innative\"
Copy-Item "scripts\*.wat" -Destination "$target\scripts\" 
Copy-Item "scripts\*.wasm" -Destination "$target\scripts\" 
Copy-Item "spec\test\core\*.wast" -Destination "$target\spec\test\core\" 

Compress-Archive -Force -Path "$target" -CompressionLevel Optimal -DestinationPath "$target.zip"
Remove-Item "$target" -Recurse
}