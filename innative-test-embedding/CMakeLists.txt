cmake_minimum_required(VERSION 3.13.4)
project(innative-test-embedding LANGUAGES C VERSION ${PROJECT_VERSION})

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

file(GLOB_RECURSE testembed_SOURCES "./*.c")

if(MSVC)
  string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
  string(REPLACE "/RTC1" "" CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
else()
  string(REPLACE "-fexceptions" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
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
  target_compile_options(innative-test-embedding PRIVATE -fno-stack-protector -O3)
  target_compile_options(innative-test-embedding-d PRIVATE -fno-stack-protector -g)
endif()

target_compile_definitions(innative-test-embedding PUBLIC NDEBUG)
target_compile_definitions(innative-test-embedding-d PUBLIC DEBUG)
target_include_directories(innative-test-embedding PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_include_directories(innative-test-embedding-d PUBLIC ${CMAKE_SOURCE_DIR}/include)

retarget_output(innative-test-embedding)
retarget_output(innative-test-embedding-d)

if(WIN32)
  # Cmake does not support linking with a static library
  set_target_properties(innative-test-embedding PROPERTIES COMPILE_PDB_NAME "$(TargetName)")
  set_target_properties(innative-test-embedding-d PROPERTIES COMPILE_PDB_NAME "$(TargetName)") 
endif()
