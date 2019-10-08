# inNative
[![inNative Discord](https://img.shields.io/badge/Discord-%23inNative-blue.svg)](https://discord.gg/teQ9Uz5)

An AOT (ahead-of-time) compiler for WebAssembly that creates **C compatible binaries**, either as **sandboxed plugins** you can dynamically load, or as **stand-alone executables** that interface *directly with the operating system*. This allows webassembly modules to participate in C linking and the build process, either statically, dynamically, or with access to the host operating system. The runtime can be installed standalone on a user's machine, or it can be embedded inside your program. It's highly customizable, letting you choose the features, isolation level, and optimization amount you need for your use-case.
 
## Community
Join a team of webassembly enthusiasts working hard to make inNative a more viable target for compilers, and building better integration options for non-web embeddings in general. We maintain an [active discord server](https://discord.gg/teQ9Uz5), and post articles from community members [on our website](https://innative.dev). If you find a bug, or your program can't compile on inNative until we implement a specific feature, [file an issue](https://github.com/innative-sdk/innative/issues/new) on GitHub so we can track the needs of developers.

### Documentation
The primary source of documentation for inNative is the [GitHub Wiki](https://github.com/innative-sdk/innative/wiki), which lists all the externally accessible functions and how to use them. This wiki *should* be kept up to date, but always double-check the source code comments if something seems amiss. Feel free to [file an issue](https://github.com/innative-sdk/innative/issues/new) if there is misleading or missing documentation.

## Installing
Precompiled binaries for common platforms are provided in [releases](https://github.com/innative-sdk/innative/releases) for those who do not want to build from source. It is **highly recommended** that you use precompiled binaries, because building from source requires building inNative's custom fork of LLVM, which can take a long time. The SDK is portable and can be unzipped to any directory, but can also be installed and registered on the target system. The provided installers will register the SDK with the system, which enables dynamic loaders to find the runtime, and register it as a `.wasm`, `.wat` and `.wast` file extension handler on windows. Even if you did not use an installer, you can always install a portable version by running `innative-cmd.exe -i` on windows or `./innative-cmd -i` on linux. Read the wiki articles for [the SDK](https://github.com/innative-sdk/innative/wiki/Install-the-SDK) and [the Redistributable](https://github.com/innative-sdk/innative/wiki/Install-the-Redistributable-Package) for more information.

### Command Line Utility
The inNative SDK comes with a command line utility with many useful features for webassembly developers.

    Usage: innative-cmd [-r] [-c] [-i [lite]] [-u] [-v] [-f FLAG...] [-l FILE] [-L FILE] [-o FILE] [-a FILE] [-d PATH] [-j PATH] [-s [FILE]] [-w [MODULE:]FUNCTION] FILE...
      -r : Run the compiled result immediately and display output. Requires a start function.
      -f <FLAG>: Set a supported flag to true. Flags:
             sandbox, homogenize, llvm, strict, whitelist, multithreaded, library, noinit, debug, check_stack_overflow, check_float_trunc, check_memory_access, check_indirect_call, check_int_division, disable_tail_call
             o0, o1, o2, os, o3, fastmath
      -l <FILE> : Links the input files against <FILE>, which must be a static library.
      -L <FILE> : Links the input files against <FILE>, which must be an ELF shared library.
      -o <FILE> : Sets the output path for the resulting executable or library.
      -a <FILE> : Specifies an alternative linker to use instead of LLD.
      -d <PATH> : Sets the directory that contains the SDK library and data files.
      -j <PATH> : Sets the directory for temporary object files and intermediate compilation results.
      -e <MODULE> : Sets the environment/system module name. Any functions with the module name will have the module name stripped when linking with C functions.
      -s [<FILE>] : Serializes all modules to .wat files. <FILE> can specify the output if only one module is present.
      -w <[MODULE:]FUNCTION> : whitelists a given C import, does name-mangling if the module is specified.
      -g : Instead of compiling immediately, creates a loader embedded with all the modules, environments, and settings, which compiles the modules on-demand when run.
      -c : Assumes the input files are actually LLVM IR files and compiles them into a single webassembly module.
      -i [lite]: Installs this SDK to the host operating system. On Windows, also updates file associations unless 'lite' is specified.
      -u : Uninstalls and deregisters this SDK from the host operating system.
      -v : Turns on verbose logging.

Example usage:

    innative-cmd your-module.wasm
    innative-cmd -r your-module.wasm
    innative-cmd yourfile.wat -f debug o3 -r
    innative-cmd your-library.wasm -f library
    
## Building
LLVM uses CMake to build, but CMake currently has issues with the nested LLD project, so build instructions are still provided on a per-platform basis. The version of LLVM used in this project is a fork with additions to LLD, so attempting to compile inNative with a different build of LLVM **will not work**.

All build steps start with `git submodule update --init --recursive`. This is **mandatory** because of nested git submodules that the project relies on. If you get errors, be sure to double check that you have acquired `llvm`, `llvm/tools/lld`, `spec`, and `spec/document/core/util/katex`.

### Windows
Regardless of whether you are using Visual Studio 2017 or 2019, **make sure you have the v141_xp toolkit installed**, first. Run `build-llvm.ps1` if you have Visual Studio 2019, or run `build-llvm.ps1 2017` if you have Visual Studio 2017, and wait for it to complete. If the script was successful, open `innative.sln` in Visual Studio and build the project, or run `msbuild innative.sln`. **Visual Studio 2019 broke MSBuild outside the developer prompt, so you'll have to open the .sln file and manually compile Debug and MinSizeRel yourself.**
 
The script downloads a portable cmake and python3 into the `bin` directory. If you would rather run the commands yourself, have existing installations of cmake/python, or want to modify the LLVM compilation flags, you can run the commands for your version of Visual Studio yourself:

    git submodule update --init --recursive
    
    mkdir -p bin/llvm
    cd bin/llvm
    
    # Visual Studio 2017
    ..\cmake-3.14.1-win32-x86\bin\cmake.exe -G "Visual Studio 15 Win64" -DLLVM_TARGETS_TO_BUILD="X86;WebAssembly" -DLLVM_INCLUDE_EXAMPLES=OFF -DLLVM_INCLUDE_TESTS=OFF -DLLVM_INCLUDE_UTILS=OFF -DLLVM_BUILD_TOOLS=ON -DCMAKE_CXX_FLAGS_RELEASE="/MT" -DCMAKE_CXX_FLAGS_MINSIZEREL="/MT" -DCMAKE_CXX_FLAGS_RELWITHDEBINFO="/MT" -DCMAKE_CXX_FLAGS_DEBUG="/MTd" -D PYTHON_EXECUTABLE="../python3/python.exe" ../../llvm
    
    # Visual Studio 2019
    ..\cmake-3.14.1-win32-x86\bin\cmake.exe -G "Visual Studio 16" "-Tv141_xp,host=x64" -DCMAKE_GENERATOR_PLATFORM="x64" -DLLVM_TARGETS_TO_BUILD="X86;WebAssembly" -DLLVM_INCLUDE_EXAMPLES=OFF -DLLVM_INCLUDE_TESTS=OFF -DLLVM_INCLUDE_UTILS=OFF -DLLVM_BUILD_TOOLS=ON -DCMAKE_CXX_FLAGS_RELEASE="/MT" -DCMAKE_CXX_FLAGS_MINSIZEREL="/MT" -DCMAKE_CXX_FLAGS_RELWITHDEBINFO="/MT" -DCMAKE_CXX_FLAGS_DEBUG="/MTd" -D PYTHON_EXECUTABLE="../python3/python.exe" ../../llvm
 
    msbuild llvm.sln /p:Configuration=MinSizeRel
    msbuild llvm.sln /p:Configuration=Debug

### Linux
**Ensure that you have `cmake` and `python` installed**, as the script can't do this for you. Run `build-llvm.sh` if you want to use `make`, or you can run `build-llvm.sh ninja` if you have `ninja` installed to compile LLVM with ninja (you'll still need to use make for inNative). If it was successful, run `make` from the top level source directory to build all inNative projects. If you would rather run the commands yourself or want to adjust the LLVM compilation flags, you can run the steps manually below:
 
    #!/bin/bash
    git submodule update --init --recursive

    mkdir -p bin/llvm
    cd bin/llvm

    # Makefiles
    cmake ../../llvm -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE:STRING="MinSizeRel" -DLLVM_TARGETS_TO_BUILD:STRING="X86;WebAssembly" -DLLVM_BUILD_LLVM_DYLIB:BOOL=OFF -DLLVM_OPTIMIZED_TABLEGEN:BOOL=ON -DLLVM_INCLUDE_EXAMPLES:BOOL=OFF -DLLVM_INCLUDE_TESTS:BOOL=OFF -DLLVM_INCLUDE_BENCHMARKS:BOOL=OFF -DLLVM_APPEND_VC_REV:BOOL=OFF
    make

    # Ninja
    cmake -GNinja ../../llvm -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE:STRING="MinSizeRel" -DLLVM_TARGETS_TO_BUILD:STRING="X86;WebAssembly" -DLLVM_BUILD_LLVM_DYLIB:BOOL=OFF -DLLVM_OPTIMIZED_TABLEGEN:BOOL=ON -DLLVM_INCLUDE_EXAMPLES:BOOL=OFF -DLLVM_INCLUDE_TESTS:BOOL=OFF -DLLVM_INCLUDE_BENCHMARKS:BOOL=OFF -DLLVM_APPEND_VC_REV:BOOL=OFF
    ninja

### Build benchmarks
The benchmarks are already compiled to webassembly, but if you want to recompile them yourself, you can run `make benchmarks` from the root directory, assuming you have a webassembly-enabled compiler available. If you are on windows, it is recommended you simply use WSL to build the benchmarks.

### Build Docker Image
A `Dockerfile` is included in the source that uses a two-stage build process to create an alpine docker image. When assembling a docker image, it is recommended you make a *shallow clone* of the repository (without any submodules) and then run `docker build .` from the root directory, without building anything. Docker will copy the repository and clone the submodules itself, before building both LLVM and inNative, which can take quite some time. Once compiled, inNative will be copied into a fresh alpine image and installed so it is usable from the command line, while the LLVM compilation result will be discarded.

A prebuilt version of this image is [available here](https://cloud.docker.com/u/blackhole12/repository/docker/blackhole12/innative)
    
## Targeting inNative
 
No compiler fully supports inNative, because current WebAssembly compilers target *web embeddings* and make assumptions about which functions are available. For now, try building webassembly modules that have no dependencies, as these can always be run on any webassembly implementation. True C interop is provided via two special compiler functions, `_innative_to_c` and `_innative_from_c`. These can be used to acquire C pointers to WebAssembly memory to pass to other functions, and to convert C pointers into a form that can be manipulated by WebAssembly. **However**, it is not possible to safely manipulate outside memory pointers, so `_innative_` pointers can only be accessed when not in strict mode. inNative also provides a [custom `cref` extension](https://github.com/innative-sdk/innative/wiki/inNative-cref-Extension) that automatically converts WebAssembly indexes into C pointers for external C functions.
 
The [WebIDL bindings proposal](https://github.com/WebAssembly/webidl-bindings) will make it easier to target native C environments, and hopefully compilers will make it easier to target non-web embeddings of WebAssembly.
 
## Embedding inNative
 
inNative is compiled as either a dynamic or static library, and can be integrated into any project as a scripting or plugin engine. While the caveats of C interop still apply, you can still use inNative to run simple webassembly scripts inside your program. How much you trust those webassembly scripts is up to you - if you want proper sandboxing, use the **whitelist** functionality to limit what C functions they can call. After linking the inNative library to your project, use the steps below to compile and call a webassembly module.
 
    // Create the environment, setting the dynamic library flag
    IRExports exports;
    innative_runtime(&exports);
    Environment* env = (*exports.CreateEnvironment)(1, 0, (!argc ? 0 : argv[0]));
    env->flags |= ENV_LIBRARY; // Add ENV_NO_INIT if you want to manually initialize and cleanup the DLL.

    // Add the script you want to compile
    int err;
    (*exports.AddModule)(env, "your_script.wasm", 0, "your_script", &err);

    // Add the default static library and any additional libraries you want to expose to the script
    err = (*exports.AddEmbedding)(env, 0, (void*)INNATIVE_DEFAULT_ENVIRONMENT, 0);

    err = (*exports.FinalizeEnvironment)(env);
    
    // Compile and dynamically load the result
    err = (*exports.Compile)(env, "your_script.out");
    void* assembly = (*exports.LoadAssembly)("your_script.out");

    // Destroy environment (no longer needed after compilation is completed)
    (*exports.DestroyEnvironment)(env);

    // Load functions and execute
    void (*update_entity)(int) = (void (*)(int))(*exports.LoadFunction)(assembly, "your_module", "update_entity");

    (*update_entity)(0);
    
    // Free assembly once finished
    (*exports.FreeAssembly)(assembly);