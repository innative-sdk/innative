New-Item -Force "innative-windows-sdk-x64\bin\" -Type Directory
New-Item -Force "innative-windows-sdk-x64\scripts\" -Type Directory
New-Item -Force "innative-windows-sdk-x64\include\innative\" -Type Directory
New-Item -Force "innative-windows-sdk-x64\spec\test\core\" -Type Directory
Copy-Item "bin\innative.dll" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative.exp" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative.lib" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative.pdb" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative-d.dll" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative-d.exp" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative-d.lib" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative-d.pdb" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative-s.lib" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative-s.pdb" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative-s-d.lib" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative-s-d.pdb" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative-env.lib" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative-env.pdb" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative-env-d.lib" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative-env-d.pdb" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative-test-s.exe" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative-cmd.exe" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative-cmd-d.exe" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative-loader.exe" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "bin\innative-loader-d.exe" -Destination "innative-windows-sdk-x64\bin\"
Copy-Item "include\innative\*" -Destination "innative-windows-sdk-x64\include\innative\"
Copy-Item "scripts\*.wat" -Destination "innative-windows-sdk-x64\scripts\" 
Copy-Item "scripts\*.wasm" -Destination "innative-windows-sdk-x64\scripts\" 
Copy-Item "spec\test\core\*.wast" -Destination "innative-windows-sdk-x64\spec\test\core\" 

Compress-Archive -Force -Path "innative-windows-sdk-x64" -CompressionLevel Optimal -DestinationPath "innative-windows-sdk-x64.zip"
Remove-Item 'innative-windows-sdk-x64' -Recurse

New-Item -Force "innative-windows-sdk-x86\bin\" -Type Directory
New-Item -Force "innative-windows-sdk-x86\scripts\" -Type Directory
New-Item -Force "innative-windows-sdk-x86\include\innative\" -Type Directory
New-Item -Force "innative-windows-sdk-x86\spec\test\core\" -Type Directory
Copy-Item "bin32\innative.dll" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative.exp" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative.lib" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative.pdb" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative-d.dll" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative-d.exp" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative-d.lib" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative-d.pdb" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative-s.lib" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative-s.pdb" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative-s-d.lib" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative-s-d.pdb" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative-env.lib" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative-env.pdb" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative-env-d.lib" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative-env-d.pdb" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative-test-s.exe" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative-cmd.exe" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative-cmd-d.exe" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative-loader.exe" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "bin32\innative-loader-d.exe" -Destination "innative-windows-sdk-x86\bin\"
Copy-Item "include\innative\*" -Destination "innative-windows-sdk-x86\include\innative\"
Copy-Item "scripts\*.wat" -Destination "innative-windows-sdk-x86\scripts\" 
Copy-Item "scripts\*.wasm" -Destination "innative-windows-sdk-x86\scripts\" 
Copy-Item "spec\test\core\*.wast" -Destination "innative-windows-sdk-x86\spec\test\core\" 

Compress-Archive -Force -Path "innative-windows-sdk-x86" -CompressionLevel Optimal -DestinationPath "innative-windows-sdk-x86.zip"
Remove-Item 'innative-windows-sdk-x86' -Recurse

Compress-Archive -Force -Path "bin\innative.dll","bin\innative-d.dll","bin\innative-s.lib","bin\innative-s-d.lib","bin\innative-env.lib","bin\innative-env-d.lib","bin\innative-cmd.exe","bin\innative-cmd-d.exe" -CompressionLevel Optimal -DestinationPath "innative-windows-runtime-x64.zip"

Compress-Archive -Force -Path "bin32\innative.dll","bin32\innative-d.dll","bin32\innative-s.lib","bin32\innative-s-d.lib","bin32\innative-env.lib","bin32\innative-env-d.lib","bin32\innative-cmd.exe","bin32\innative-cmd-d.exe" -CompressionLevel Optimal -DestinationPath "innative-windows-runtime-x86.zip"