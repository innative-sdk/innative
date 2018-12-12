# inNative
[![inNative Discord](https://img.shields.io/badge/Discord-%23inNative-blue.svg)](https://discord.gg/)

A *non-web embedding* of WebAssembly that provides a **native platform** for running WebAssembly **inside the host operating system**. inNative is an AOT (ahead-of-time) compiler that uses LLVM to compile webassembly modules into **C compatible binaries**. This allows webassembly modules to participate in C linking, either as static libraries, shared libraries, or even executables.
 
## Community
Join a team of webassembly enthusiasts working hard to make inNative a more viable target for compilers, and building better integration options for non-web embeddings in general. We maintain an [active discord server](https://discord.gg/), and post articles from community members [on our website](https://innative.io). If you find a bug, or your program can't compile on inNative until we implement a specific feature, [file an issue](https://github.com/innative-sdk/innative/issues/new) on GitHub so we can track the needs of developers.

## Installing
Precompiled binaries for common platforms are provided in [releases](https://github.com/innative-sdk/innative/releases) for those who do not want to build from source. The SDK is portable and can be unzipped to any directory, but can also be installed to integrate it with the system. The provided installers will register the SDK with the system, which enables dynamic loaders to find the runtime, and register it as a `.wasm`, `.wat` and `.wast` file extension handler on windows. Even if you did not use an installer, you can always install a portable version by running `innative-cmd.exe -install` on windows or `./innative-cmd -install` on linux.
 
## Building
LLVM uses CMake to build, but CMake currently has issues with the nested LLD project, so build instructions are still provided on a per-platform basis. The version of LLVM used in this project is a **fork** with some minor changes, so attempting to compile inNative with a different build of LLVM might not work. Ninja has been unreliable when trying to compile both LLVM and LLD at the moment, so the steps below use either Visual Studio 2017 or Unix Makefiles to build LLVM+LLD.
 
All build steps start with `git submodule update --init --recursive`. This is critical because of nested git submodules that the project relies on. If you get errors, be sure to double check that you have acquired `llvm`, `llvm/tools/lld`, `spec`, and `spec/document/core/util/katex`.
 
### Windows
Run `build-llvm.ps1`, and wait for it to complete. If the script was successful, open `innative.sln` in Visual Studio and build the project, or run `msbuild innative.sln`
 
The script downloads a portable cmake and python3 into the `bin` directory. If you would rather run the commands yourself, have existing installations of cmake/python, or want to modify the LLVM compilation flags, you can instead run these steps in a Visual Studio Developer Prompt (which can find msbuild for you) from the root directory:
 
    git submodule update --init --recursive
    
    mkdir -p bin/llvm
    cd bin/llvm
   
    ..\cmake-3.11.4-win32-x86\bin\cmake.exe -G "Visual Studio 15 Win64" -DLLVM_TARGETS_TO_BUILD="X86" -DLLVM_INCLUDE_EXAMPLES=OFF -DLLVM_INCLUDE_TESTS=OFF -DLLVM_INCLUDE_UTILS=OFF -DLLVM_BUILD_TOOLS=ON -DCMAKE_CXX_FLAGS_RELEASE="/MT" -DCMAKE_CXX_FLAGS_MINSIZEREL="/MT" -DCMAKE_CXX_FLAGS_RELWITHDEBINFO="/MT" -DCMAKE_CXX_FLAGS_DEBUG="/MTd" -D PYTHON_EXECUTABLE="../python3/python.exe" ../../llvm

    msbuild llvm.sln /p:Configuration=MinSizeRel
    msbuild llvm.sln /p:Configuration=Debug

### Linux
Ensure that you have `cmake` and `python` installed, as the script can't do this for you. Run `build-llvm.sh` and wait for it to complete. If it was successful, run `make` from the top level source directory to build all innative projects. If you would rather run the commands yourself or want to adjust the LLVM compilation flags, you can run the steps manually below:
 
    git submodule update --init --recursive

    mkdir -p bin/llvm
    cd bin/llvm

    cmake ../../llvm -DLLVM_TARGETS_TO_BUILD:STRING="X86" -DLLVM_BUILD_LLVM_DYLIB:BOOL=ON -DLLVM_OPTIMIZED_TABLEGEN:BOOL=ON
    make

    cd lib
    g++ -Wl,-whole-archive liblldCOFF.a liblldCommon.a liblldCore.a liblldDriver.a liblldELF.a liblldMachO.a liblldMinGW.a liblldReaderWriter.a liblldWasm.a liblldYAML.a -shared -o liblld-8svn.so -Wl,-no-whole-archive -L. -lpthread -lLLVM-8svn 

    cp libLLVM-8svn.so ../../
    cp liblld-8svn.so ../../

    
## Targeting inNative
 
No compiler fully supports inNative, because current WebAssembly compilers target *web embeddings* and make assumptions about which functions are available. For now, try building webassembly modules that have no dependencies, as these can always be run on any webassembly implementation. True C interop is provided via two special compiler functions, `_innative_to_c` and `_innative_from_c`. These can be used to acquire C pointers to WebAssembly memory to pass to other functions, and to convert C pointers into a form that can be manipulated by WebAssembly. **However**, it is not possible to safely manipulate outside memory pointers, so `_innative_` pointers can only be accessed when not in strict mode.
 
The host-object proposal will make it easier to target native C environments, and hopefully compilers will make it easier to target non-web embeddings of WebAssembly.
 
## Embedding inNative
 
inNative is compiled as either a dynamic or static library, and can be integrated into any project as a scripting or plugin engine. While the caveats of C interop still apply, you can still use inNative to run simple webassembly scripts inside your program. How much you trust those webassembly scripts is up to you - if you want proper sandboxing, use the **whitelist** functionality to limit what C functions they can call. After linking the inNative library to your project, use the steps below to compile and call a webassembly module.
 
    // Create the environment, setting the dynamic library flag
    IRExports exports;
    innative_runtime(&exports);
    int flags = ENV_DLL | ENV_STRICT | ENV_WHITELIST;
    Environment* env = (*exports.CreateEnvironment)(flags, 1, 0, argv[0]);

    // Add the script you want to compile
    int err;
    (*exports.AddModule)(env, "your_script.wasm", 0, "your_script", &err);

    // Add the default static library and any additional libraries you want to expose to the script
    int err = (*exports.AddEmbedding)(env, 0, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0);
 
    // Compile and dynamically load the result
    err = (*exports.Compile)(env, "your_script.out");
    void* assembly = (*exports.LoadAssembly)(flags, "your_script.out");

    // Destroy environment
    (*exports.DestroyEnvironment)(env);

    // Load functions and execute
    void (*update_entity)(int) = (void (*)(int))(*exports.LoadFunction)(assembly, "your_module", "update_entity");

    (*update_entity)(0); 