cmake_minimum_required(VERSION 3.13.4)
if(WIN32)
  project(innative-env LANGUAGES C ASM_MASM VERSION ${PROJECT_VERSION})
else()
  project(innative-env LANGUAGES C VERSION ${PROJECT_VERSION})
  find_package(Threads REQUIRED)
endif()

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
  string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
  string(REPLACE "/RTC1" "" CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
endif()

# add library and include directories
add_library(innative-env STATIC ${env_SOURCES})
add_library(innative-env-d STATIC ${env_SOURCES})

set_property(TARGET innative-env PROPERTY C_STANDARD 17)
set_property(TARGET innative-env-d PROPERTY C_STANDARD 17)
set_property(TARGET innative-env PROPERTY VERBOSE_MAKEFILE TRUE)
set_property(TARGET innative-env-d PROPERTY VERBOSE_MAKEFILE TRUE)

# Directly set target compilation settings to bypass Release/Debug modes
set(CMAKE_BUILD_TYPE "")

if(MSVC)
  target_compile_options(innative-env PRIVATE /WX- /GS-)
  target_compile_options(innative-env-d PRIVATE /Zi /Od /Ob0 /Gd /Gm- /GS-)
else()
  target_compile_options(innative-env PRIVATE -O3 -msse -msse2 -msse3 -mmmx -m3dnow -mcx16 -fno-stack-protector -Wno-unused-function ${C_WARNINGS})
  target_compile_options(innative-env-d PRIVATE -O3 -g3 -fno-stack-protector -Wno-unused-function ${C_WARNINGS})
endif()

target_compile_definitions(innative-env PUBLIC NDEBUG)
target_compile_definitions(innative-env-d PUBLIC DEBUG)
target_include_directories(innative-env PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_include_directories(innative-env-d PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_target_properties(innative-env PROPERTIES DEBUG_POSTFIX "" POSITION_INDEPENDENT_CODE ON)
set_target_properties(innative-env-d PROPERTIES DEBUG_POSTFIX "" POSITION_INDEPENDENT_CODE ON)
retarget_lib(innative-env TRUE)
retarget_lib(innative-env-d TRUE)

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
