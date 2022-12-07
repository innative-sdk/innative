cmake_minimum_required(VERSION 3.15)
if(WIN32)
project(innative LANGUAGES CXX ASM_MASM VERSION ${PROJECT_VERSION})
else()
project(innative LANGUAGES CXX VERSION ${PROJECT_VERSION})
endif()

include(CheckSymbolExists)
include(CMakePushCheckState)
  
find_package(LLVM REQUIRED CONFIG HINTS ${LLVM_HINTS})
find_package(Clang REQUIRED)
find_package(LLD REQUIRED)

if(USE_MIMALLOC)
  find_package(mimalloc CONFIG REQUIRED)
endif()

list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(HandleLLVMOptions)

add_definitions(${LLVM_DEFINITIONS})
set(inNative_EX_SOURCES "")

if(WIN32)
  list(APPEND inNative_EX_SOURCES "x86_64.asm" "innative.rc")
else()
  find_package(Threads REQUIRED)
  find_package(ZLIB REQUIRED)
  if(ZLIB_FOUND)
    # Check if zlib we found is usable; for example, we may have found a 32-bit
    # library on a 64-bit system which would result in a link-time failure.
    cmake_push_check_state()
    set(CMAKE_REQUIRED_INCLUDES ${ZLIB_INCLUDE_DIRS})
    set(CMAKE_REQUIRED_LIBRARIES ${ZLIB_LIBRARY})
    check_symbol_exists(compress2 zlib.h HAVE_ZLIB)
    cmake_pop_check_state()
  endif()
  find_library(LIB_RT rt REQUIRED)
endif()

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

file(GLOB_RECURSE inNative_SOURCES "./*.cpp")

if(MSVC)
  string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
else()
  string(REPLACE "-fexceptions" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
endif()

# add library and include directories
add_library(innative ${inNative_SOURCES} ${inNative_EX_SOURCES})
target_include_directories(innative PRIVATE ${LLVM_INCLUDE_DIRS})
target_include_directories(innative PUBLIC ${CMAKE_SOURCE_DIR}/include)

# Append LLD libraries because LLD doesn't have CMake info and remove libraries we don't need
llvm_map_components_to_libnames(llvm_libs orcjit passes option objcarcopts coverage support coroutines frontendopenmp native RISCV AArch64 WebAssembly X86 debuginfocodeview debuginfodwarf debuginfogsym debuginfomsf debuginfopdb LTO WindowsManifest LibDriver)
list(APPEND llvm_libs lldCOFF lldCommon lldCore lldDriver lldELF lldMachO lldMinGW lldReaderWriter lldWasm lldYAML)

standard_target_flags(innative)
retarget_output(innative)
    
# Configure precompiled headers
if(MSVC)
  set(PCH_PATH "llvm.h-${CMAKE_GENERATOR_PLATFORM}.pch")
  set_source_files_properties("llvm.cpp"
                              PROPERTIES COMPILE_FLAGS "/Yc\"llvm.h\" /Fp\"${PCH_PATH}\""
                                         OBJECT_OUTPUTS "${PCH_PATH}")
                                         
  set_source_files_properties("atomic_instructions.cpp" "compile.cpp" "debug.cpp" "debug_dwarf.cpp" "debug_pdb.cpp" "debug_sourcemap.cpp" "debug_wat.cpp" "dwarf_parser.cpp" "instructions.cpp" "intrinsic.cpp" "jit.cpp" "link.cpp" "reverse.cpp" "optimize.cpp"
                              PROPERTIES COMPILE_FLAGS "/Yu\"llvm.h\" /Fp\"${PCH_PATH}\"")
else()
  if(${CMAKE_VERSION} VERSION_GREATER "3.15.999")
    target_precompile_headers(innative PRIVATE "llvm.h")
  endif()
endif()

# Link against LLVM libraries
if(NOT WIN32)
  list(APPEND llvm_libs "-Wl,--no-whole-archive")
  list(INSERT llvm_libs 0 "-Wl,--whole-archive")
endif()

set_target_properties(innative PROPERTIES COMPILE_PDB_NAME "$(TargetName)") 
target_link_libraries(innative PRIVATE ${llvm_libs})
target_link_directories(innative PRIVATE ${LLVM_LIBRARY_DIR})

if(USE_MIMALLOC)
  target_link_libraries(innative PRIVATE mimalloc-static)
endif()

if(WIN32)
  # Cmake does not support linking with a static library
  if(NOT BUILD_SHARED_LIBS)
    set_target_properties(innative PROPERTIES OUTPUT_NAME "innative-s")
    
    foreach(X IN LISTS LLVM_AVAILABLE_LIBS)
      SET(CMAKE_STATIC_LINKER_FLAGS "${CMAKE_STATIC_LINKER_FLAGS} \"${LLVM_LIBRARY_DIR}/${X}.lib\"")
    endforeach()
  endif()
  set_source_files_properties("innative.rc" PROPERTIES LANGUAGE RC)
  set_source_files_properties("x86_64.asm" PROPERTIES LANGUAGE ASM_MASM)
else()
  target_link_libraries(innative PUBLIC ${LIB_RT} ZLIB::ZLIB ${Threads} ${CMAKE_DL_LIBS})
endif()
