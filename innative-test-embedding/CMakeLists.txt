cmake_minimum_required(VERSION 3.13.4)
project(innative-test-embedding LANGUAGES C VERSION ${PROJECT_VERSION})

file(GLOB_RECURSE testembed_SOURCES "./*.c")

if(MSVC)
  string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
  string(REPLACE "/RTC1" "" CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
endif()

# add library and include directories
add_library(innative-test-embedding STATIC ${testembed_SOURCES})
add_library(innative-test-embedding-d STATIC ${testembed_SOURCES})

# Directly set target compilation settings to bypass Release/Debug modes
set(CMAKE_BUILD_TYPE "")

if(MSVC)
  target_compile_options(innative-test-embedding PRIVATE /WX- /GS-)
  target_compile_options(innative-test-embedding-d PRIVATE /Zi /Od /Ob0 /Gd /Gm- /GS-)
else()
  target_compile_options(innative-test-embedding PRIVATE -fno-stack-protector -O3 ${C_WARNINGS})
  target_compile_options(innative-test-embedding-d PRIVATE -fno-stack-protector -g ${C_WARNINGS})
endif()

target_compile_definitions(innative-test-embedding PRIVATE NDEBUG)
target_compile_definitions(innative-test-embedding-d PRIVATE DEBUG)
target_include_directories(innative-test-embedding PRIVATE ${CMAKE_SOURCE_DIR}/include)
target_include_directories(innative-test-embedding-d PRIVATE ${CMAKE_SOURCE_DIR}/include)
set_target_properties(innative-test-embedding PROPERTIES 
  DEBUG_POSTFIX "" 
  POSITION_INDEPENDENT_CODE ON
  C_STANDARD 17
  VERBOSE_MAKEFILE TRUE
)
set_target_properties(innative-test-embedding-d PROPERTIES 
  DEBUG_POSTFIX ""
  POSITION_INDEPENDENT_CODE ON 
  C_STANDARD 17 
  VERBOSE_MAKEFILE TRUE
)
retarget_lib(innative-test-embedding TRUE)
retarget_lib(innative-test-embedding-d TRUE)

if(WIN32)
  # Cmake does not support linking with a static library
  set_target_properties(innative-test-embedding PROPERTIES COMPILE_PDB_NAME "$(TargetName)")
  set_target_properties(innative-test-embedding-d PROPERTIES COMPILE_PDB_NAME "$(TargetName)") 
endif()
