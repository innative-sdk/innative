# inNative
[![inNative Discord](https://img.shields.io/badge/Discord-%23inNative-blue.svg)](https://discord.gg/teQ9Uz5)
[![Build Status](https://travis-ci.org/innative-sdk/innative.svg?branch=master)](https://travis-ci.org/innative-sdk/innative)

An AOT (ahead-of-time) compiler for WebAssembly that creates **C compatible binaries**, either as **sandboxed plugins** you can dynamically load, or as **stand-alone executables** that interface *directly with the operating system*. This allows webassembly modules to participate in C linking and the build process, either statically, dynamically, or with access to the host operating system. The runtime can be installed standalone on a user's machine, or it can be embedded inside your program. It's highly customizable, letting you choose the features, isolation level, and optimization amount you need for your use-case. If you find a bug, or your program can't compile on inNative until we implement a specific feature, [file an issue](https://github.com/innative-sdk/innative/issues/new) on GitHub so we can track the needs of developers.

### Documentation
The primary source of documentation for inNative is the [GitHub Wiki](https://github.com/innative-sdk/innative/wiki), which lists all the externally accessible functions and how to use them. This wiki *should* be kept up to date, but always double-check the source code comments if something seems amiss. Feel free to [file an issue](https://github.com/innative-sdk/innative/issues/new) if there is misleading or missing documentation.

## Installing
Precompiled binaries for Windows are provided in [releases](https://github.com/innative-sdk/innative/releases) for those who do not want to build from source. The provided installers will register the runtime with the system, which enables dynamic loaders to find the runtime, and register it as a `.wasm`, `.wat` and `.wast` file extension handler on windows. Even if you did not use an installer, you can always install a portable version by running `innative-cmd.exe -i` on windows. Read the wiki articles for [the Redistributable](https://github.com/innative-sdk/innative/wiki/Install-the-Redistributable-Package) for more information.

For Linux, you should use cmake to install the binaries after they have been compiled. See below for instructions on building from source.

### Command Line Utility
The inNative SDK comes with a command line utility with many useful features for webassembly developers.

    Usage: innative-cmd [-r] [-f <FLAG>] [-l <FILE> ... ] [-shared-lib <FILE> ... ] [-o <FILE>] [-serialize [<FILE>|emitdebug]] [-generate-loader] [-v] [-build-sourcemap] [-w <[MODULE:]FUNCTION> ... ] [-sys <MODULE>] [-start <[MODULE:]FUNCTION>] [-linker] [-i [lite]] [-u] [-sdk <DIR>] [-obj <DIR>] [-compile-llvm] [-abi <ABI>] [-cpu] [-cpu-feature <SUBFEATURE> ... ] [-arch <ARCH>]
      -r -run: Run the compiled result immediately and display output. Requires a start function.
      -f -flag -flags <FLAG>: Set a supported flag to true.
        Flags:
        strict
        sandbox
        whitelist
        multithreaded
        debug
        debug_pdb
        debug_dwarf
        library
        llvm
        noinit
        check_stack_overflow
        check_float_trunc
        check_memory_access
        check_indirect_call
        check_int_division
        disable_tail_call
        o0
        o1
        o2
        o3
        os
        fastmath

      -l -lib -libs -library <FILE> ... : Links the input files against <FILE>, which must be a static library.
      -shared-lib -shared-libs -shared-library <FILE> ... : Links the input files against <FILE>, which must be an ELF shared library.
      -o -out -output <FILE>: Sets the output path for the resulting executable or library.
      -serialize [<FILE>|emitdebug]: Serializes all modules to .wat files in addition to compiling them. <FILE> can specify the output if only one module is present, or 'emitdebug' will emit debug information
      -generate-loader: Instead of compiling immediately, creates a loader embedded with all the modules, environments, and settings, which compiles the modules on-demand when run.
      -v -verbose: Turns on verbose logging.
      -build-sourcemap: Assumes input files are ELF object files or binaries that contain DWARF debugging information, and creates a source map from them.
      -w -whitelist <[MODULE:]FUNCTION> ... : whitelists a given C import, does name-mangling if the module is specified.
      -sys -system <MODULE>: Sets the environment/system module name. Any functions with the module name will have the module name stripped when linking with C functions
      -start <[MODULE:]FUNCTION>: Sets or overrides the start function of a given module, in case it hasn't been properly specified. The function can't take any parameters and must return void.
      -linker: Specifies an alternative linker executable to use instead of LLD.
      -i -install [lite]: (WINDOWS ONLY) Installs this SDK to the host operating system and updates file associations unless 'lite' is specified.
      -u -uninstall: (WINDOWS ONLY) Uninstalls and deregisters this SDK from the host operating system.
      -sdk -library-dir <DIR>: Sets the directory that contains the SDK library and data files.
      -obj -obj-dir -object-dir -intermediate-dir <DIR>: Sets the directory for temporary object files and intermediate compilation results.
      -compile-llvm: Assumes the input files are LLVM IR files and compiles them into a single webassembly module.
      -abi -platform <ABI>: Set the target ABI platform to compile for.
        ABIs:
        windows
        sys-v
        linux
        freebsd
        solaris
        arm

      -cpu -cpu-name: Set the target CPU name for code optimization. Set to "generic" for maximum portability (subject to CPU features requested). If this option isn't specified, the host CPU will be targeted.
      -cpu-feature -cpu-features <SUBFEATURE> ... : List CPU subfeatures, like SSSE3 or AVX, that the compiler should assume exist. Must be a valid subfeature string that LLVM recognizes.
      -arch -architecture <ARCH>: Set the target CPU architecture to compile for.
        ARCHs:
        x86
        amd64
        
        
    Example usage:

      innative-cmd your-module.wasm
      innative-cmd -r your-module.wasm
      innative-cmd yourfile.wat -flag debug o3 -run
      innative-cmd your-library.wasm -f library
      innative-cmd your-module.wasm -abi windows -arch x86
    
## Building
If you'd like to run the test suite, make sure you include the webassembly spec submodule by running `git clone --recurse-submodules`. If you get errors when running the tests, be sure to double check that you have acquired `spec` and `spec/document/core/util/katex`.

### Windows
For those building from source on Windows, we use a [custom vcpkg registry](https://github.com/Fundament-Software/vcpkg), which is already configured for you via `vcpkg-configuration.json`. You can either enable `vcpkg` integration on windows by running `vcpkg integrate install` and opening `innative.sln`, or you can use cmake to build the project, with `cmake [SOURCE ROOT] -DCMAKE_TOOLCHAIN_FILE=[VCPKG LOCATION]\scripts\buildsystems\vcpkg.cmake -DVCPKG_TARGET_TRIPLET=x64-windows-static`. Both of these options should use the `vcpkg.json` manifest mode, which will automatically use our custom registry, or you can install the dependencies manually after copying `vcpkg-configuration.json` to your vcpkg root. You can also use this custom registry to incorporate innative into another build pipeline via vcpkg.

### Linux
Since inNative requires C++17 to build, the minimum supported compiler is gcc-7 or clang-5. Either use the provided nix flake to build via nix by running `nix build .?submodules=1`, or install LLVM 13 and Python from your package manager. Then run `cmake` to create makefiles or a Ninja configuration that you can then use to build the project. It is suggested to create a new folder called `build` and then run `cmake ..` to isolate the generated project files.

## Targeting inNative
To build a shared library that does not rely on WASI, you can use `wasm_malloc.c` and clang:

    clang your_program.c wasm_malloc.c -o your_program.wasm --target=wasm32-unknown-unknown-wasm -nostdlib --optimize=3 -Xlinker --no-entry -Xlinker --export-dynamic

inNative supports sourcemaps and, if present, will generate debug info for the original language that was compiled to WebAssembly. With C++, inNative can automatically extract the debug information generated by clang: simply add `-g` (and remove optimizations), and the resulting wasm module will have the necessary debug information embedded inside of it. Compile this webassembly module with the DEBUG flag enabled and inNative will automatically generate debugging information from it.

    clang your_program.c wasm_malloc.c -g -o your_program.wasm --target=wasm32-unknown-unknown-wasm -nostdlib --optimize=0 -Xlinker --no-entry -Xlinker --export-dynamic

No compiler fully supports inNative, because current WebAssembly compilers target *web embeddings* and make assumptions about which functions are available. For now, try building webassembly modules that have no dependencies, as these can always be run on any webassembly implementation. True C interop is provided via two special compiler functions, `_innative_to_c` and `_innative_from_c`. These can be used to acquire C pointers to WebAssembly memory to pass to other functions, and to convert C pointers into a form that can be manipulated by WebAssembly. **However**, it is not possible to safely manipulate outside memory pointers, so using these intrinsics can invalidate the sandbox, and by default you must enable them explicitly using the C Import whitelist. inNative also provides a [custom `cref` extension](https://github.com/innative-sdk/innative/wiki/inNative-cref-Extension) that automatically converts WebAssembly indexes into C pointers for external C functions.
 
The [WebIDL bindings proposal](https://github.com/WebAssembly/webidl-bindings) will make it easier to target native C environments, and hopefully compilers will make it easier to target non-web embeddings of WebAssembly.
 
## Embedding inNative
 
inNative is compiled as either a dynamic or static library, and can be integrated into any project as a scripting or plugin engine. While the caveats of C interop still apply, you can still use inNative to run simple webassembly scripts inside your program. How much you trust those webassembly scripts is up to you - if you want proper sandboxing, use the **whitelist** functionality to limit what C functions they can call. After linking the inNative library to your project, use the steps below to compile and call a webassembly module.
 
    // Create the environment, setting the dynamic library flag
    INExports exports;
    innative_runtime(&exports);
    Environment* env = (*exports.CreateEnvironment)(1, 0, (!argc ? 0 : argv[0]));
    env->flags |= ENV_LIBRARY; // Add ENV_NO_INIT if you want to manually initialize and cleanup the DLL.

    // Add the script you want to compile
    int err;
    (*exports.AddModule)(env, "your_script.wasm", 0, "your_script", &err);

    // Add the default static library and any additional libraries you want to expose to the script
    err = (*exports.AddEmbedding)(env, 0, (void*)(*exports.GetDefaultEmbedding)(false), 0, nullptr);

    err = (*exports.FinalizeEnvironment)(env);
    
    // Compile and dynamically load the result
    err = (*exports.Compile)(env, "your_script.out");
    void* assembly = (*exports.LoadAssembly)("your_script.out");

    // Destroy environment (no longer needed after compilation, but will be needed if using JIT)
    (*exports.DestroyEnvironment)(env);

    // Load functions and execute
    void (*update_entity)(int) = (void (*)(int))(*exports.LoadFunction)(assembly, "your_module", "update_entity");

    (*update_entity)(0);
    
    // Free assembly once finished
    (*exports.FreeAssembly)(assembly);
