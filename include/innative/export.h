// Copyright (c)2019 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef __EXPORT_H__IN__
#define __EXPORT_H__IN__

#include "innative/schema.h"

#ifdef IN_PLATFORM_WIN32
#define IN_STATIC_EXTENSION ".lib"
#define IN_LIBRARY_FLAG ""
#define IN_LIBRARY_EXTENSION ".dll"
#define IN_EXE_EXTENSION ".exe"
#define IN_WIN32_REGPATH L"SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\innative-cmd.exe"
#else
#define IN_STATIC_EXTENSION ".a"
#define IN_LIBRARY_FLAG "-l"
#define IN_LIBRARY_EXTENSION ".so"
#define IN_EXE_EXTENSION ""
#endif

#ifdef IN_DEBUG
#define INNATIVE_DEFAULT_ENVIRONMENT "innative-env-d" IN_STATIC_EXTENSION
#else
#define INNATIVE_DEFAULT_ENVIRONMENT "innative-env" IN_STATIC_EXTENSION
#endif

#define IN_INIT_FUNCTION "_innative_internal_start"
#define IN_EXIT_FUNCTION "_innative_internal_exit"

#ifdef  __cplusplus
extern "C" {
#endif
  typedef void(*IN_Entrypoint)();

  // These tags determine the kind of embedding file that's being provided to the environment
  enum IN_EMBEDDING_TAGS
  {
    IN_TAG_ANY = 0, // Automatically determine the embedding type
    IN_TAG_STATIC, // Static library
    IN_TAG_DYNAMIC // Dynamic (shared) library
  };

  // Allows C code to access a webassembly global, provided it knows the correct type.
  typedef union __IN_GLOBAL_TYPE
  {
    uint32_t i32;
    uint64_t i64;
    float f32;
    double f64;
    void* memory; // The additional indirection for memory is important here, becuase the global is a pointer to a pointer
  } IRGlobal;

  // Contains pointers to the actual runtime functions
  typedef struct __IN_EXPORTS
  {
    /// Creates an environment with default settings, which must be destroyed using DestroyEnvironment
    /// \param modules The number of modules that are expected to be added to the environment - can just be an estimate.
    /// \param maxthreads The maximum number of threads to use for multithreading. 0 defaults to logical cores.
    /// \param arg0 the first argument sent to the program. Used to determine binary location on POSIX systems.
    Environment* (*CreateEnvironment)(unsigned int modules, unsigned int maxthreads, const char* arg0);

    /// Adds a module to an environment. This could happen asyncronously if multithreading is enabled, in which case
    // 'err' won't be valid until FinalizeEnvironment() is called to resolve all pending module loads.
    /// \param env The environment to modify.
    /// \param data Either a pointer to the module in memory, or a UTF8 encoded null-terminated string pointing to a file that contains the module.
    /// \param size The length of the memory that the data pointer points to, or zero if the data pointer is actually a UTF8 encoded null terminated string.
    /// \param name A name to use for the module. If the module data does not contain a name, this will be used.
    /// \param err A pointer to an integer that recieves an error code should the function fail. Not valid until FinalizeEnvironment() is called.
    void(*AddModule)(Environment* env, const void* data, uint64_t size, const char* name, int* err); // If size is 0, data points to a null terminated UTF8 file path

    /// Adds a whitelist entry to the environment. This will only be used if the whitelist is enabled via the flags.
    /// \param env The environment to modify.
    /// \param module_name The name of a module, in case the C function is actually a name-mangled webassembly function. This parameter should be null for standard C functions.
    /// \param export_name The name of the function to add to the whitelist. Must be a valid UTF8 webassembly function name.
    void(*AddWhitelist)(Environment* env, const char* module_name, const char* export_name);

    /// Adds an embedding to the environment. This is usually a static or shared C library that exposes C functions that the webassembly modules can call.
    /// \param env The environment to modify.
    /// \param tag An IN_EMBEDDING_TAGS enumeration option that provides an optional hint for the type of file. To automatically choose, set to 0.
    /// \param data Either a pointer to memory, or a UTF8 encoded null-terminated string pointing to a file.
    /// \param size The length of the memory that the data pointer points to. If size is 0, the data pointer is actually a null terminated UTF8 encoded file path.
    enum IN_ERROR(*AddEmbedding)(Environment* env, int tag, const void* data, uint64_t size);

    /// Finalizes the environment, blocking until all modules have finished loading (in case of any asyncronous loads) and ensures all configuration data is loaded.
    /// \param env The environment to finalize
    enum IN_ERROR(*FinalizeEnvironment)(Environment* env);

    /// Compiles all the modules in the environment using the current configuration and any cached results into a binary file.
    /// \param env The environment to compile.
    /// \param file The path of the output file that is produced.
    enum IN_ERROR(*Compile)(Environment* env, const char* file);

    /// Loads a webassembly binary (usually a dynamic library) produced by Compile into memory, allowing you to load functions and other exported symbols.
    /// \param file the path of the file to load.
    void* (*LoadAssembly)(const char* file);
    
    /// Frees a webassembly binary, unloading it from memory and performing any cleanup required.
    /// \param assembly A pointer to a webassembly binary loaded by LoadAssembly.
    void(*FreeAssembly)(void* assembly);

    /// Gets a function from a webassembly binary that has been loaded into memory. This function cannot determine the type signature, you must cast it manually to the correct function type.
    /// \param assembly A pointer to a webassembly binary loaded by LoadAssembly.
    /// \param module_name The name of the module the function is exported from.
    /// \param function The name of the function.
    IN_Entrypoint(*LoadFunction)(void* assembly, const char* module_name, const char* function); // if function is null, loads the entrypoint function

    /// Gets a function pointer from a table, given the specified index. This function has no bounds checking and is only safe if you know how many entries the table has.
    /// \param assembly A pointer to a webassembly binary loaded by LoadAssembly.
    /// \param module_name The name of the module the table is exported from.
    /// \param function The name of the table the function pointer belongs to.
    /// \param index The index of the function pointer. This function doesn't know how big the table is, and will go out of bounds if the index is invalid.
    IN_Entrypoint(*LoadTable)(void* assembly, const char* module_name, const char* table, varuint32 index);

    /// Gets a pointer to a global from a webassembly binary loaded by LoadAssembly, which can potentially be modified if it is mutable (but this function cannot determine whether it was intended to be mutable).
    /// \param assembly A pointer to a webassembly binary loaded by LoadAssembly.
    /// \param module_name The name of the module the global is exported from.
    /// \param export_name The name of the global that has been exported.
    IRGlobal*(*LoadGlobal)(void* assembly, const char* module_name, const char* export_name);

    /// Clears the environment's cached compilation of a given module, or clears the entire cache if the module is a null pointer.
    /// \param env The environment to clear.
    /// \param m An optional module whose compilation cache will be cleared. If this is a null pointer, instead clears the entire cache of the environment.
    void(*ClearEnvironmentCache)(Environment* env, Module* m);

    /// Destroys an environment and safely deconstructs all it's caches and memory allocations.
    /// \param env The environment to destroy.
    void(*DestroyEnvironment)(Environment* env);
  } IRExports;

  /// Statically linked function that loads the runtime stub, which then loads the actual runtime functions into exports.
  /// \param exports Pointer to an existing IRExports structure that will be filled with function pointers.
  IN_COMPILER_DLLEXPORT extern void innative_runtime(IRExports* exports);

  // --- Tooling functions that exist for command line utilities that always statically link to the runtime ---

  /// Compiles and executes a .wast script using the given environment. This execution will modify the environment and add all
  /// modules referenced in the script according to the registration rules.
  /// \param data Either a pointer to the script in memory, or a UTF8 encoded null-terminated string pointing to a file that contains the script.
  /// \param sz The length of the memory that the data pointer points to, or zero if the data pointer is actually a UTF8 encoded null terminated string.
  /// \param env The environment to execute the script with. This environment can potentially be modified by the script being executed.
  /// \param always_compile By default, this function only performs a compilation when necessary to call a function. If this is set to true,
  ///        all modules are compiled even if no function inside them is called.
  /// \param output Sets the output directory where compilation results should be stored. Intermediate results will still be in env->objpath
  IN_COMPILER_DLLEXPORT extern int innative_compile_script(const uint8_t* data, size_t sz, Environment* env, bool always_compile, const char* output);
  
  /// Serializes the 'm'th module in the given environment into the provided output file.
  /// \param env The environment that contains the module that will be serialized.
  /// \param m The index of the in the given environment that will be serialized.
  /// \param out The path to the output file that will contain the serialized result.
  IN_COMPILER_DLLEXPORT extern int innative_serialize_module(Environment* env, size_t m, const char* out);

  /// Convenience function that sets the working directory to the executable's root directory.
  /// \param arg0 the first argument sent to the program. Used to determine binary location on POSIX systems.
  IN_COMPILER_DLLEXPORT extern void innative_set_work_dir_to_bin(const char* arg0);

  /// Installs this runtime, usually only called by innative-cmd, which makes assumptions about file locations.
  /// \param arg0 the first argument sent to the program. Used to determine binary location on POSIX systems.
  /// \param full On windows, performs a "full" installation, which associates the runtime with .wat/.wast/.wasm files.
  IN_COMPILER_DLLEXPORT extern int innative_install(const char* arg0, bool full);

  /// Uninstalls whatever runtime version this is from the operating system.
  IN_COMPILER_DLLEXPORT extern int innative_uninstall();

  /// Returns the string representation of a TYPE_ENCODING enumeration, or NULL if the lookup fails. Useful for debuggers.
  /// \param type_encoding The TYPE_ENCODING value to get the string representation of.
  IN_COMPILER_DLLEXPORT extern const char* innative_type_encoding_string(int type_encoding);

  /// Returns the string representation of an IN_ERROR enumeration, or NULL if the lookup fails. Useful for debuggers.
  /// \param error_code The IN_ERROR code to get the string representation of.
  IN_COMPILER_DLLEXPORT extern const char* innative_error_string(int error_code);
  
  /// Performs a reverse compilation of LLVM IR into WebAssembly using the built-in LLVM version in this runtime.
  /// \param files An array of UTF8 file paths to compile.
  /// \param n The length of the 'files' array.
  /// \param flags Environment flags to compile with. This is mostly used to set the ENV_LIBRARY flag in case this should be a shared library.
  /// \param out The output file that will store the compilation result.
  /// \param log A C FILE* stream that should be used for logging errors or warnings.
  IN_COMPILER_DLLEXPORT extern int innative_compile_llvm(const char** files, size_t n, int flags, const char* out, FILE* log);

#ifdef  __cplusplus
}
#endif

#endif