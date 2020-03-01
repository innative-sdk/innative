// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#ifndef IN__EXPORT_H
#define IN__EXPORT_H

#include "innative/schema.h"

#ifdef IN_PLATFORM_WIN32
  #define IN_STATIC_EXTENSION  ".lib"
  #define IN_LIBRARY_FLAG      ""
  #define IN_LIBRARY_EXTENSION ".dll"
  #define IN_EXE_EXTENSION     ".exe"
  #define IN_WIN32_REGPATH     L"SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\innative-cmd.exe"
#else
  #define IN_STATIC_EXTENSION  ".a"
  #define IN_LIBRARY_FLAG      "-l"
  #define IN_LIBRARY_EXTENSION ".so"
  #define IN_EXE_EXTENSION     ""
#endif

#ifdef IN_DEBUG
  #define INNATIVE_DEFAULT_ENVIRONMENT "innative-env-d" IN_STATIC_EXTENSION
#else
  #define INNATIVE_DEFAULT_ENVIRONMENT "innative-env" IN_STATIC_EXTENSION
#endif

#define IN_INIT_FUNCTION "_innative_internal_start"
#define IN_EXIT_FUNCTION "_innative_internal_exit"

#ifdef __cplusplus
extern "C" {
#endif
typedef void (*IN_Entrypoint)();

// These tags determine the kind of embedding file that's being provided to the environment
enum IN_EMBEDDING_TAGS
{
  IN_TAG_ANY = 0, // Automatically determine the embedding type
  IN_TAG_STATIC,  // Static library
  IN_TAG_DYNAMIC  // Dynamic (shared) library
};

typedef struct IN__TABLE_ENTRY
{
  IN_Entrypoint func;
  varuint32 type;
} INTableEntry;

typedef struct IN__TABLE
{
  INTableEntry* entries;
  uint64_t size;
} INTable;

typedef struct IN__MEMORY
{
  void* bytes;
  uint64_t size;
} INMemory;

// Allows C code to access a WebAssembly global, provided it knows the correct type.
typedef union IN__GLOBAL_TYPE
{
  uint32_t i32;
  uint64_t i64;
  float f32;
  double f64;
  INMemory memory; // The additional indirection for memory is important here, becuase the global is a pointer to a pointer
  INTable table;
} INGlobal;

// Stores metadata about a WebAssembly module compiled into the binary
typedef struct IN__MODULE_METADATA
{
  const char* name;
  varuint32 version;
  varuint32 n_tables;
  INTable** tables;
  varuint32 n_memories;
  INMemory** memories;
  varuint32 n_globals;
  INGlobal** globals;
  varuint32 n_functions;
  IN_Entrypoint* functions;
} INModuleMetadata;

// Contains pointers to the actual runtime functions
typedef struct IN__EXPORTS
{
  /// Creates an environment with default settings, which must be destroyed using DestroyEnvironment
  /// \param modules The number of modules that are expected to be added to the environment - can just be an estimate.
  /// \param maxthreads The maximum number of threads to use for multithreading. 0 defaults to logical cores.
  /// \param arg0 the first argument sent to the program. Used to determine binary location on POSIX systems.
  Environment* (*CreateEnvironment)(unsigned int modules, unsigned int maxthreads, const char* arg0);

  /// Adds a module to an environment. This could happen asynchronously if multithreading is enabled, in which case
  // 'err' won't be valid until FinalizeEnvironment() is called to resolve all pending module loads.
  /// \param env The environment to modify.
  /// \param data Either a pointer to the module in memory, if size is non-zero, or a UTF8 encoded null-terminated string
  /// pointing to a file that contains the module.
  /// \param size The length of the memory that the data pointer points to, or zero if the data pointer is actually a UTF8
  /// encoded null terminated string.
  /// \param name A name to use for the module. If the module data does not contain a name, this will be used.
  /// \param err A pointer to an integer that receives an error code should the function fail. Not valid until
  /// FinalizeEnvironment() is called.
  void (*AddModule)(Environment* env, const void* data, size_t size, const char* name, int* err);

  /// Adds a prebuilt module object to the environment. This happens synchronously, regardless of multithreading flags.
  /// \param env The environment to modify.
  /// \param m The module to add to the environment. This must be a valid Module object or the validation step will fail.
  /// The module will be copied into the environment - further modifications to the given module pointer won't affect the
  /// environment's internal copy.
  int (*AddModuleObject)(Environment* env, const Module* m);

  /// Adds a whitelist entry to the environment. This will only be used if the whitelist is enabled via the ENV_WHITELIST
  /// flag. \param env The environment to modify. \param module_name The name of a module, in case the C function is
  /// actually a name-mangled WebAssembly function. This parameter should be null for standard C functions. \param
  /// export_name The name of the function to add to the whitelist. Must be a valid UTF8 WebAssembly function name.
  enum IN_ERROR (*AddWhitelist)(Environment* env, const char* module_name, const char* export_name);

  /// Adds an embedding to the environment. This is usually a static or shared C library that exposes C functions that the
  /// WebAssembly modules can call.
  /// \param env The environment to modify.
  /// \param tag An IN_EMBEDDING_TAGS enumeration option that provides an optional hint for the type of file. To
  ///  automatically choose, set to 0.
  /// \param data Either a pointer to memory, or a UTF8 encoded null-terminated string pointing to a file.
  /// \param size The length of the memory that the
  /// data pointer points to. If size is 0, the data pointer is actually a null terminated UTF8 encoded file path.
  /// \param name_override Resolves all functions in this embedded library as raw C functions with the given module name.
  enum IN_ERROR (*AddEmbedding)(Environment* env, int tag, const void* data, size_t size, const char* name_override);

  /// Tells the linker to export the given symbol from the resulting binary.
  /// \param env The environment to modify.
  /// \param symbol The null-terminated name of the symbol to export.
  enum IN_ERROR (*AddCustomExport)(Environment* env, const char* symbol);

  /// Finalizes the environment, blocking until all modules have finished loading (in case of any asynchronous loads) and
  /// ensures all configuration data is loaded.
  /// \param env The environment to finalize
  enum IN_ERROR (*FinalizeEnvironment)(Environment* env);

  /// Validates all the modules in the environment using the current configuration.
  /// \param env The environment to verify.
  enum IN_ERROR (*Validate)(Environment* env);

  /// Compiles and verifies all the modules in the environment using the current configuration and any cached results into a
  /// binary file.
  /// \param env The environment to compile.
  /// \param file The path of the output file that is produced.
  enum IN_ERROR (*Compile)(Environment* env, const char* file);

  /// Loads a WebAssembly binary (usually a dynamic library) produced by Compile into memory, allowing you to load functions
  /// and other exported symbols.
  /// \param file the path of the file to load.
  void* (*LoadAssembly)(const char* file);

  /// Frees a WebAssembly binary, unloading it from memory and performing any cleanup required.
  /// \param assembly A pointer to a WebAssembly binary loaded by LoadAssembly.
  void (*FreeAssembly)(void* assembly);

  /// Gets a function from a WebAssembly binary that has been loaded into memory. This function cannot determine the type
  /// signature, you must cast it manually to the correct function type.
  /// \param assembly A pointer to a WebAssembly binary loaded by LoadAssembly.
  /// \param module_name The name of the module the function is exported from.
  /// \param function The name of the function. If null, loads the entrypoint function.
  IN_Entrypoint (*LoadFunction)(void* assembly, const char* module_name, const char* function);

  /// Gets a function pointer from a table, given the specified index. If the index is out of bounds, returns null.
  /// \param assembly A pointer to a WebAssembly binary loaded by LoadAssembly.
  /// \param module_name The name of the module the table is exported from.
  /// \param function The name of the table the function pointer belongs to.
  /// \param index The index of the function pointer. If this is out of bounds, the function returns null.
  IN_Entrypoint (*LoadTable)(void* assembly, const char* module_name, const char* table, varuint32 function_index);

  /// Gets a pointer to a global from a WebAssembly binary loaded by LoadAssembly, which can potentially be modified if it
  /// is mutable (but this function cannot determine whether it was intended to be mutable).
  /// \param assembly A pointer to a WebAssembly binary loaded by LoadAssembly.
  /// \param module_name The name of the module the global is exported from.
  /// \param export_name The name of the global that has been exported.
  INGlobal* (*LoadGlobal)(void* assembly, const char* module_name, const char* export_name);

  /// Gets the metadata associated with the module at the given zero-based index.
  /// \param assembly A pointer to a WebAssembly binary loaded by LoadAssembly.
  /// \param module_index A zero-based index. If this is out-of-bounds, the function returns null.
  INModuleMetadata* (*GetModuleMetadata)(void* assembly, uint32_t module_index);

  /// Gets a function pointer from a table, given the specified index. This function has bounds checking.
  /// \param assembly A pointer to a WebAssembly binary loaded by LoadAssembly.
  /// \param module_index The index of the module the table is exported from.
  /// \param table_index The index of the table the function pointer belongs to.
  /// \param function_index The index of the function pointer.
  IN_Entrypoint (*LoadTableIndex)(void* assembly, uint32_t module_index, uint32_t table_index, varuint32 function_index);

  /// Gets a global value at the specified index, or returns null if the index is out of bounds.
  /// \param assembly A pointer to a WebAssembly binary loaded by LoadAssembly.
  /// \param module_index The index of the module the table is exported from.
  /// \param global_index The index of the global to retrieve.
  INGlobal* (*LoadGlobalIndex)(void* assembly, uint32_t module_index, uint32_t global_index);

  /// Gets a linear memory global at the specified index, or returns null if the index is out of bounds.
  /// \param assembly A pointer to a WebAssembly binary loaded by LoadAssembly.
  /// \param module_index The index of the module the table is exported from.
  /// \param global_index The index of the linear memory to retrieve.
  INGlobal* (*LoadMemoryIndex)(void* assembly, uint32_t module_index, uint32_t memory_index);

  /// Searches for an exported function with the given name in a table and replaces the function pointer with another.
  /// \param assembly A pointer to a WebAssembly binary loaded by LoadAssembly.
  /// \param module_index The index of the module the table is exported from.
  /// \param table_index The index of the table to search through.
  /// \param function Name of the exported function to search the table for.
  /// \param replace function pointer to another function that should replace the function's table entry
  int (*ReplaceTableFuncPtr)(void* assembly, uint32_t module_index, uint32_t table_index, const char* function,
                             IN_Entrypoint replace);

  /// Clears the environment's cached compilation of a given module, or clears the entire cache if the module is a null
  /// pointer.
  /// \param env The environment to clear.
  /// \param m An optional module whose compilation cache will be cleared. If this is a null pointer, instead clears the
  /// entire cache of the environment.
  void (*ClearEnvironmentCache)(Environment* env, Module* m);

  /// Returns the string representation of a TYPE_ENCODING enumeration, or NULL if the lookup fails. Useful for debuggers.
  /// \param type_encoding The TYPE_ENCODING value to get the string representation of.
  const char* (*GetTypeEncodingString)(int type_encoding);

  /// Returns the string representation of an IN_ERROR enumeration, or NULL if the lookup fails. Useful for debuggers.
  /// \param error_code The IN_ERROR code to get the string representation of.
  const char* (*GetErrorString)(int error_code);

  /// Destroys an environment and safely deconstructs all it's caches and memory allocations.
  /// \param env The environment to destroy.
  void (*DestroyEnvironment)(Environment* env);

  /// Compiles and executes a .wast script using the given environment. This execution will modify the environment and add
  /// all modules referenced in the script according to the registration rules.
  /// \param data Either a pointer to the script in memory, or a UTF8 encoded null-terminated string pointing to a file that
  /// contains the script.
  /// \param sz The length of the memory that the data pointer points to, or zero if the data pointer is actually a UTF8
  /// encoded null terminated string.
  /// \param env The environment to execute the script with. This environment can potentially be modified by the script
  /// being executed.
  /// \param always_compile By default, this function only performs a compilation when necessary to call a function. If this
  /// is set to true, all modules are compiled even if no function inside them is called.
  /// \param output Sets the output directory where compilation results should be stored. Intermediate results will still be
  /// in env->objpath
  int (*CompileScript)(const uint8_t* data, size_t sz, Environment* env, bool always_compile, const char* output);

  /// Serializes the 'm'th module in the given environment into the provided output file.
  /// \param env The environment that contains the module that will be serialized.
  /// \param m The index of the in the given environment that will be serialized.
  /// \param out If 'len' is zero, the path to an output file. Otherwise, a pointer to an in-memory buffer that will contain
  /// the serialized result. If null, defaults to a file called '<module_name>.wat' in the current working directory
  /// \param len If 'len' is nonzero and out is not null, then this should be the length of the buffer pointed to by out
  /// \param emitdebug If true, emits any per-instruction debug information associated with the given module.
  int (*SerializeModule)(Environment* env, size_t m, const char* out, size_t* len, bool emitdebug);

  /// Loads a source map from the given path or memory location into the module at index 'm'
  /// \param env The environment that contains the module the sourcemap will be attached to.
  /// \param m the index of the module to attache the sourcemap to.
  /// \param path if len is zero, a null-terminated UTF8 string containing the path of the sourcemap. Otherwise, a pointer
  /// to a memory location.
  /// \param len if zero, path points to a string. Otherwise, this contains the size of the memory location holding the
  /// sourcemap.
  int (*LoadSourceMap)(Environment* env, unsigned int m, const char* path, size_t len);

  /// Serializes the given source map to JSON and saves it at path.
  /// \param map A pointer to the source map that should be serialized.
  /// \param path A null-terminated UTF8 string pointing to the file location where the source map should be saved
  enum IN_ERROR (*SerializeSourceMap)(const SourceMap* map, const char* path);

  /// Inserts a new, zero'd element into the given module section at the specified index. It is up to the caller to
  /// initialize the new element with a valid state.
  /// \param env The environment associated with the given module.
  /// \param m A pointer to a module associated with the given environment that the section should be inserted into.
  /// \param section A enumeration value signifying what section of the module to insert a new element into.
  /// \param index The index, relative to the section being modified, where the new element should be inserted. This index
  /// cannot be greater than the size of the section being modified, but it can be equal to the current size of the section,
  /// which will simply append the new item to the end.
  int (*InsertModuleSection)(Environment* env, Module* m, enum WASM_MODULE_SECTIONS section, varuint32 index);

  /// Deletes an element from the given module section at the specified index and moves the other elements in the array.
  /// \param env The environment associated with the given module.
  /// \param m A pointer to a module associated with the given environment that the section should be removed from.
  /// \param section A enumeration value signifying what section of the module to remove an element from.
  /// \param index The index, relative to the section being modified, of the element that will be removed.
  int (*DeleteModuleSection)(Environment* env, Module* m, enum WASM_MODULE_SECTIONS section, varuint32 index);

  /// Resizes a ByteArray to the size of the provided memory buffer and copies the entire memory section into it's internal
  /// buffer.
  /// \param env The environment associated with the given bytearray.
  /// \param bytearray The ByteArray object whose internal buffer will recieve the data.
  /// \param data A pointer to a location in memory which will be copied to the ByteArray's internal buffer.
  /// \param size The size of the memory location to copy into the ByteArray's internal buffer.
  int (*SetByteArray)(Environment* env, ByteArray* bytearray, const void* data, varuint32 size);

  /// Copies the given null-terminated string into the internal buffer of the target identifier, preserving the
  /// null-terminator.
  /// \param env The environment associated with the given identifier.
  /// \param identifier The identifier whose buffer will be set to the given string.
  /// \param str A null-terminated string to be copied into the identifier.
  int (*SetIdentifier)(Environment* env, Identifier* identifier, const char* str);

  /// Inserts a local group definition into the given function body at the provided index and initializes it with a type,
  /// and an optional debuginfo value.
  /// \param env The environment associated with the given function.
  /// \param body The FunctionBody object to insert the local definition into.
  /// \param index The index where the new local will be inserted. This refers to local groups, NOT the actual local index.
  /// \param local The type that will be inserted.
  /// \param count How many locals of the given type will be in this local group.
  /// \param info An optional pointer to debug information describing the local.
  int (*InsertModuleLocal)(Environment* env, FunctionBody* body, varuint32 index, varsint7 local, varuint32 count,
                           DebugInfo* info);

  // Removes a local definition from the given function body at the provided index.
  /// \param env The environment associated with the given function.
  /// \param body The FunctionBody object to remove the local definition from.
  /// \param index The index of the local that will be removed.
  int (*RemoveModuleLocal)(Environment* env, FunctionBody* body, varuint32 index);

  /// Inserts an instruction into the given function body at the provided index and initializes it with an initial value.
  /// \param env The environment associated with the given function.
  /// \param func The FunctionBody object to insert the instruction into.
  /// \param index The index where the new instruction will be inserted.
  /// \param ins The instruction that will be inserted.
  int (*InsertModuleInstruction)(Environment* env, FunctionBody* body, varuint32 index, Instruction* ins);

  /// Removes an instruction from the given function body at the provided index.
  /// \param env The environment associated with the given function.
  /// \param func The FunctionBody object to remove the instruction from.
  /// \param index The index of the instruction that will be removed.
  int (*RemoveModuleInstruction)(Environment* env, FunctionBody* body, varuint32 index);

  /// Inserts a parameter into the given function type at the provided index and initializes it. If both the corresponding
  /// function body and a DebugInfo object are provided, also inserts and initializes the appropriate debug information.
  /// \param env The environment associated with the given function.
  /// \param func The FunctionType object to insert the parameter into.
  /// \param desc The corresponding FunctionDesc object to add the debug information to.
  /// \param param The parameter that will be inserted.
  /// \param info An optional pointer to debug information describing the parameter.
  int (*InsertModuleParam)(Environment* env, FunctionType* func, FunctionDesc* desc, varuint32 index, varsint7 param,
                           DebugInfo* info);

  /// Removes a parameter from the given function type at the provided index. If the corresponding function body is
  /// provided, also removes the corresponding DebugInfo object from the body, if it exists.
  /// \param env The environment associated with the given function.
  /// \param func The FunctionType object to remove the parameter from.
  /// \param desc The corresponding FunctionDesc object to remove the debug information from.
  /// \param index The index of the parameter that will be removed.
  int (*RemoveModuleParam)(Environment* env, FunctionType* func, FunctionDesc* desc, varuint32 index);

  /// Inserts and initializes a return value into the given function type at the provided index.
  /// \param env The environment associated with the given function.
  /// \param func The FunctionType object to insert the result value into.
  /// \param result The result that will be inserted.
  int (*InsertModuleReturn)(Environment* env, FunctionType* func, varuint32 index, varsint7 result);

  /// Removes a return value from the given function type at the provided index.
  /// \param env The environment associated with the given function.
  /// \param func The FunctionType object to remove the result value from.
  /// \param index The index of the result value that will be removed.
  int (*RemoveModuleReturn)(Environment* env, FunctionType* func, varuint32 index);
} INExports;

/// Statically linked function that loads the runtime stub, which then loads the actual runtime functions into exports.
/// \param exports Pointer to an existing INExports structure that will be filled with function pointers.
IN_COMPILER_DLLEXPORT extern void innative_runtime(INExports* exports);

// --- Tooling functions that exist for command line utilities that always statically link to the runtime ---

/// Convenience function that sets the working directory to the executable's root directory.
/// \param arg0 the first argument sent to the program. Used to determine binary location on POSIX systems.
IN_COMPILER_DLLEXPORT extern void innative_set_work_dir_to_bin(const char* arg0);

/// Installs this runtime, usually only called by innative-cmd, which makes assumptions about file locations.
/// \param arg0 the first argument sent to the program. Used to determine binary location on POSIX systems.
/// \param full On windows, performs a "full" installation, which associates the runtime with .wat/.wast/.wasm files.
IN_COMPILER_DLLEXPORT extern int innative_install(const char* arg0, bool full);

/// Uninstalls whatever runtime version this is from the operating system.
IN_COMPILER_DLLEXPORT extern int innative_uninstall();

/// Performs a reverse compilation of LLVM IR into WebAssembly using the built-in LLVM version in this runtime.
/// \param files An array of UTF8 file paths to compile.
/// \param n The length of the 'files' array.
/// \param flags Environment flags to compile with. This is mostly used to set the ENV_LIBRARY flag in case this should be a
/// shared library.
/// \param out The output file that will store the compilation result.
/// \param log A C FILE* stream that should be used for logging errors or warnings.
IN_COMPILER_DLLEXPORT extern int innative_compile_llvm(const char** files, size_t n, int flags, const char* out, FILE* log);

#ifdef __cplusplus
}
#endif

#endif