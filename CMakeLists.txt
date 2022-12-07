cmake_minimum_required(VERSION 3.15)
project(innative_sdk VERSION 0.2.0)
option(BUILD_SHARED_LIBS "enable shared library" ON)
option(USE_MIMALLOC "Replace the default allocator with mimalloc" OFF)

if(MSVC)
  # This ensures that we default to static but let vcpkg configure things the way it wants
  set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>$<$<STREQUAL:${VCPKG_CRT_LINKAGE},dynamic>:DLL>" CACHE STRING "")
else()
  set(CPP_WARNINGS -Wall -Wno-attributes -Wno-unknown-pragmas -Wno-missing-braces -Wno-unused-function -Wno-comment -Wno-char-subscripts -Wno-sign-compare -Wno-unused-variable -Wno-switch -Wno-parentheses)
endif()

if (MSVC)
  if("${MSVC_C_ARCHITECTURE_ID}" STREQUAL "X86")
    set(TARGET_ARCH "i686")
  elseif("${MSVC_C_ARCHITECTURE_ID}" STREQUAL "x64")
    set(TARGET_ARCH "x86_64")
  elseif("${MSVC_C_ARCHITECTURE_ID}" STREQUAL "ARM")
    set(TARGET_ARCH "arm")
  else()
    message(FATAL_ERROR "Failed to determine the MSVC target architecture: ${MSVC_C_ARCHITECTURE_ID}")
  endif()
else()
  execute_process(
    COMMAND ${CMAKE_C_COMPILER} -dumpmachine
    RESULT_VARIABLE RESULT
    OUTPUT_VARIABLE TARGET_ARCH
    ERROR_QUIET
  )
  if (RESULT)
    message(FATAL_ERROR "Failed to determine target architecture triplet: ${RESULT}")
  endif()
  string(REGEX MATCH "([^-]+).*" ARCH_MATCH ${TARGET_ARCH})
  if (NOT CMAKE_MATCH_1 OR NOT ARCH_MATCH)
    message(FATAL_ERROR "Failed to match the target architecture triplet: ${TARGET_ARCH}")
  endif()
  set(TARGET_ARCH ${CMAKE_MATCH_1})
endif()
message(STATUS "Target architecture - ${TARGET_ARCH}")

if(TARGET_ARCH MATCHES "i386")
  set(TARGET_ARCH "x86")
elseif(TARGET_ARCH MATCHES "i486")
  set(TARGET_ARCH "x86")
elseif(TARGET_ARCH MATCHES "i586")
  set(TARGET_ARCH "x86")
elseif(TARGET_ARCH MATCHES "i686")
  set(TARGET_ARCH "x86")
elseif(TARGET_ARCH MATCHES "amd64")
  set(TARGET_ARCH "x64")
elseif(TARGET_ARCH MATCHES "x86_64")
  set(TARGET_ARCH "x64")
elseif(TARGET_ARCH MATCHES "xscale")
  set(TARGET_ARCH "arm")
elseif(TARGET_ARCH MATCHES "ARM")
  set(TARGET_ARCH "arm")
elseif(TARGET_ARCH MATCHES "powerpc")
  set(TARGET_ARCH "ppc")
elseif(TARGET_ARCH MATCHES "powerpcspe")
  set(TARGET_ARCH "ppc")
elseif(TARGET_ARCH MATCHES "powerpc64")
  set(TARGET_ARCH "ppc64")
elseif(TARGET_ARCH MATCHES "ppu")
  set(TARGET_ARCH "ppc64")
elseif(TARGET_ARCH MATCHES "risc-v")
  set(TARGET_ARCH "riscv")
elseif(TARGET_ARCH MATCHES "riscv32")
  set(TARGET_ARCH "riscv")
else()
  message(FATAL_ERROR, "Can't figure out what architecture to build for!")
endif()
  
if (CMAKE_SYSTEM_NAME MATCHES "Windows")
    set(BIN_DIR "windows")
elseif(CMAKE_SYSTEM_NAME MATCHES "Linux")
    set(BIN_DIR "linux")
elseif(CMAKE_SYSTEM_NAME MATCHES "Darwin")
    set(BIN_DIR "darwin")
elseif(CMAKE_SYSTEM_NAME MATCHES "Android")
    set(BIN_DIR "android")
elseif(CMAKE_SYSTEM_NAME MATCHES "FreeBSD")
    set(BIN_DIR "freebsd")
else()
  message(FATAL_ERROR, "Can't figure out what platform we're on!")
endif()

set(BIN_DIR "bin-${BIN_DIR}-${TARGET_ARCH}")

function(retarget_output target)
  if(NOT USE_DEFAULT_FOLDERS)
    set_target_properties(${target} PROPERTIES DEBUG_POSTFIX "-d")
    set_target_properties(${target}
        PROPERTIES
        VS_DEBUGGER_WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        ARCHIVE_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        LIBRARY_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        RUNTIME_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        ARCHIVE_OUTPUT_DIRECTORY_RELEASE "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        LIBRARY_OUTPUT_DIRECTORY_RELEASE "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        RUNTIME_OUTPUT_DIRECTORY_RELEASE "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        ARCHIVE_OUTPUT_DIRECTORY_MINSIZEREL "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        LIBRARY_OUTPUT_DIRECTORY_MINSIZEREL "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        RUNTIME_OUTPUT_DIRECTORY_MINSIZEREL "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        ARCHIVE_OUTPUT_DIRECTORY_RELWITHDEBINFO "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        LIBRARY_OUTPUT_DIRECTORY_RELWITHDEBINFO "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        RUNTIME_OUTPUT_DIRECTORY_RELWITHDEBINFO "${CMAKE_SOURCE_DIR}/${BIN_DIR}"
        PREFIX ""
    )
  endif()
endfunction()


function(standard_target_flags target)
  set_property(TARGET ${target} PROPERTY C_STANDARD 17)
  set_property(TARGET ${target} PROPERTY CXX_STANDARD 17)
  set_property(TARGET ${target} PROPERTY CXX_EXTENSIONS OFF)
  set_property(TARGET ${target} PROPERTY CXX_STANDARD_REQUIRED ON)
  set_property(TARGET ${target} PROPERTY VERBOSE_MAKEFILE TRUE)

  if(MSVC)
    target_compile_options(${target} PRIVATE /Zc:preprocessor $<$<CONFIG:Release>:/Oi /Ot /GL> ${CPP_WARNINGS})
  else()
    target_compile_options(${target} PRIVATE -fno-exceptions -fno-rtti -Wno-reorder $<IF:$<CONFIG:Debug>,-g,-O3 -msse -msse2 -msse3 -mmmx -m3dnow -mcx16> ${CPP_WARNINGS})
    target_compile_definitions(${target} PUBLIC $<IF:$<CONFIG:Debug>,DEBUG,NDEBUG>)
  endif()
endfunction()


add_subdirectory(innative)
add_subdirectory(innative-env)
add_subdirectory(innative-cmd)
add_subdirectory(innative-stub)
#add_subdirectory(innative-loader)
add_subdirectory(innative-assemblyscript)
add_subdirectory(innative-test-embedding)
add_subdirectory(innative-test)
