cmake_minimum_required(VERSION 3.13.4)
if(WIN32)
project(innative-env LANGUAGES C ASM_MASM VERSION ${PROJECT_VERSION})
else()
project(innative-env LANGUAGES C VERSION ${PROJECT_VERSION})
endif()

option(DYNAMIC_RUNTIME "if true, dynamically links (/MD) to the C++ runtime on MSVC. Otherwise, statically links (/MT)" OFF)

if(USE32bit)
  set(BIN_DIR "bin-Win32")
else()
  set(BIN_DIR "bin")
endif()

set(CMAKE_VERBOSE_MAKEFILE FALSE)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

list(APPEND env_SOURCES
    atomics.c
    internal.c
    wait_list.c
)

if(WIN32)
  if(USE32bit)
    list(APPEND env_SOURCES "memmove.x86.asm" "memcpy.x86.asm" "memset.x86.asm" "chkstk.x86.asm" "win32_x86.c")
   else()
    list(APPEND env_SOURCES "memcmp.amd64.asm" "memcpy.amd64.asm" "memset.amd64.asm" "chkstk.amd64.asm")
   endif()
endif()

if(MSVC)
  set(RUNTIME_FLAG "MT")
  if(DYNAMIC_RUNTIME)
    set(RUNTIME_FLAG "MD")
  endif()

  string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
else()
  string(REGEX REPLACE "-fexceptions" "" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-exceptions")
endif()

# add library and include directories
add_library(innative-env STATIC ${env_SOURCES})
add_library(innative-env-d STATIC ${env_SOURCES})

# Directly set target compilation settings to bypass Release/Debug modes
set(CMAKE_BUILD_TYPE "")

if(MSVC)
  target_compile_options(innative-env PRIVATE /D "NDEBUG" /${RUNTIME_FLAG} /WX- /GS-)
  target_compile_options(innative-env-d PRIVATE /D "DEBUG" /Zi /Od /Ob0 /${RUNTIME_FLAG}d /Gd /Gm- /GS-)
else()
  target_compile_options(innative-env PRIVATE -fno-stack-protector -DNDEBUG -O3 -msse -msse2 -msse3 -mmmx -m3dnow -mcx16)
  target_compile_options(innative-env-d PRIVATE -fno-stack-protector -DDEBUG -g)
endif()

target_include_directories(innative-env PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_include_directories(innative-env-d PUBLIC ${CMAKE_SOURCE_DIR}/include)

set_target_properties(innative-env
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

set_target_properties(innative-env-d
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

if(WIN32)
  # Cmake does not support linking with a static library
  set_target_properties(innative-env PROPERTIES COMPILE_PDB_NAME "$(TargetName)")
  set_target_properties(innative-env-d PROPERTIES COMPILE_PDB_NAME "$(TargetName)") 
  SET(CMAKE_STATIC_LINKER_FLAGS "${CMAKE_STATIC_LINKER_FLAGS} \"kernel32.lib\"")

  if(USE32bit)
    set_source_files_properties("memmove.x86.asm" "memcpy.x86.asm" "memset.x86.asm" "chkstk.x86.asm" PROPERTIES LANGUAGE ASM_MASM)
  else()
    set_source_files_properties("memcmp.amd64.asm" "memcpy.amd64.asm" "memset.amd64.asm" "chkstk.amd64.asm" PROPERTIES LANGUAGE ASM_MASM)
  endif()
endif()