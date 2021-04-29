cmake_minimum_required(VERSION 3.13.4)
if(WIN32)
project(innative LANGUAGES CXX ASM_MASM VERSION ${PROJECT_VERSION})
else()
project(innative LANGUAGES CXX VERSION ${PROJECT_VERSION})
endif()

include(CheckSymbolExists)
include(CMakePushCheckState)

set(CMAKE_PREFIX_PATH "../${BIN_DIR}/llvm;${CMAKE_PREFIX_PATH}")
  
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

add_definitions(${LLVM_DEFINITIONS})
set(inNative_EX_SOURCES "")

# For if we are building directly off the LLVM source
# get_target_property(LLVM_INCLUDE_DIRS LLVMSupport INCLUDE_DIRECTORIES)
# macro(get_llvm_version_component input component)
#   string(REGEX MATCH "${component} ([0-9]+)" match ${input})
#   if (NOT match)
#     message(FATAL_ERROR "Cannot find LLVM version component '${component}'")
#   endif()
#   set (${component} ${CMAKE_MATCH_1})
# endmacro()
# 
# file(READ ${PATH_TO_LLVM}/CMakeLists.txt LLVM_CMAKELISTS)
# get_llvm_version_component("${LLVM_CMAKELISTS}" LLVM_VERSION_MAJOR)
# get_llvm_version_component("${LLVM_CMAKELISTS}" LLVM_VERSION_MINOR)
# get_llvm_version_component("${LLVM_CMAKELISTS}" LLVM_VERSION_PATCH)
# set (LLVM_VERSION ${LLVM_VERSION_MAJOR}.${LLVM_VERSION_MINOR}.${LLVM_VERSION_PATCH})

# list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")
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
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /${RUNTIME_FLAG}d")
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /Oi /Ot /GL /${RUNTIME_FLAG}")
  set(CMAKE_CXX_FLAGS_MINSIZEREL ${CMAKE_CXX_FLAGS_RELEASE})
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO ${CMAKE_CXX_FLAGS_RELEASE})
else()
  string(REPLACE "-fexceptions" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-exceptions -fno-rtti ${CPP_WARNINGS} -Wno-reorder")
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -DDEBUG")
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -msse -msse2 -msse3 -mmmx -m3dnow -mcx16 -DNDEBUG")
  set(CMAKE_CXX_FLAGS_MINSIZEREL ${CMAKE_CXX_FLAGS_RELEASE})
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO ${CMAKE_CXX_FLAGS_RELEASE})
endif()

# add library and include directories
add_library(innative ${inNative_SOURCES} ${inNative_EX_SOURCES})
target_include_directories(innative PRIVATE ${LLVM_INCLUDE_DIRS})
#target_include_directories(innative PRIVATE ${LLD_INCLUDE_DIRS})
target_include_directories(innative PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_target_properties(innative PROPERTIES OUTPUT_NAME_DEBUG "innative-d")

# Append LLD libraries because LLD doesn't have CMake info and remove libraries we don't need
list(REMOVE_ITEM LLVM_AVAILABLE_LIBS "LLVM-C" "FrontendOpenMP" "Remarks")
if(WIN32)
  list(REMOVE_ITEM LLVM_AVAILABLE_LIBS "LTO")
endif()
list(APPEND LLVM_AVAILABLE_LIBS "lldCOFF" "lldCommon" "lldCore" "lldDriver" "lldELF" "lldMachO" "lldMinGW" "lldReaderWriter" "lldWasm" "lldYAML" "Polly" "PollyISL" "PollyPPCG")

set_target_properties(innative
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
    ARCHIVE_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
    LIBRARY_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
    RUNTIME_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
    ARCHIVE_OUTPUT_DIRECTORY_MINSIZEREL "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
    LIBRARY_OUTPUT_DIRECTORY_MINSIZEREL "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
    RUNTIME_OUTPUT_DIRECTORY_MINSIZEREL "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
)
    
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
target_link_libraries(innative PRIVATE -Wl,--whole-archive ${LLVM_AVAILABLE_LIBS} -Wl,--no-whole-archive)
target_link_directories(innative PRIVATE ${LLVM_LIBRARY_DIR})

if(WIN32)
  # Cmake does not support linking with a static library
  if(NOT BUILD_SHARED_LIBS)
    set_target_properties(innative PROPERTIES COMPILE_PDB_NAME "$(TargetName)") 
    set_target_properties(innative PROPERTIES OUTPUT_NAME "innative-s")
    set_target_properties(innative PROPERTIES OUTPUT_NAME_DEBUG "innative-s-d")
    
    foreach(X IN LISTS LLVM_AVAILABLE_LIBS)
      SET(CMAKE_STATIC_LINKER_FLAGS "${CMAKE_STATIC_LINKER_FLAGS} \"${LLVM_LIBRARY_DIR}/${X}.lib\"")
    endforeach()
  endif()
  
  set_source_files_properties("innative.rc" PROPERTIES LANGUAGE RC)
  set_source_files_properties("x86_64.asm" PROPERTIES LANGUAGE ASM_MASM)
else()
  target_link_libraries(innative PUBLIC ${LIB_RT} ${ZLIB} ${Threads} ${CMAKE_DL_LIBS})
endif()
