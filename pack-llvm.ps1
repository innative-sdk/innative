param([string]$arch="x64")

switch ($arch) {
  "x86-64" {$bin = "bin"}
  "x64" {$bin = "bin"; $arch="x86-64"}
  "x86" {$bin = "bin-Win32"}
  "arm" {$bin = "bin-ARM"}
}

$target = "llvm-10.0.0-$arch-windows"
New-Item -Force "$target\" -Type Directory
New-Item -Force "$target\MinSizeRel\lib\" -Type Directory
New-Item -Force "$target\include\" -Type Directory
New-Item -Force "$target\bin\" -Type Directory
robocopy "$bin\llvm\include" "$target\include" *.h *.inc *.def *.td *.modulemap /S
robocopy "llvm-project\llvm\include" "$target\include" *.h *.inc *.def *.td *.modulemap /S
robocopy "llvm-project\lld\include" "$target\include" *.h *.inc *.def *.td *.modulemap /S
robocopy "llvm-project\polly\include" "$target\include" *.h *.inc *.def *.td *.modulemap /S
Copy-Item "$bin\llvm\bin\*" -Destination "$target\bin\"
Copy-Item "$bin\llvm\MinSizeRel\lib\*.lib" -Destination "$target\MinSizeRel\lib\"

Compress-Archive -Force -Path $target -CompressionLevel Optimal -DestinationPath "$target.zip"
Remove-Item $target -Recurse

