cmake_minimum_required(VERSION 3.13.4)
project(innative-assemblyscript LANGUAGES C VERSION ${PROJECT_VERSION})

file(GLOB_RECURSE assemblyscript_SOURCES "./*.c")

if(MSVC)
  string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
  string(REPLACE "/RTC1" "" CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
endif()

# add library and include directories
add_library(innative-assemblyscript STATIC ${assemblyscript_SOURCES})
add_library(innative-assemblyscript-d STATIC ${assemblyscript_SOURCES})

set_property(TARGET innative-assemblyscript PROPERTY C_STANDARD 17)
set_property(TARGET innative-assemblyscript-d PROPERTY C_STANDARD 17)
set_property(TARGET innative-assemblyscript PROPERTY VERBOSE_MAKEFILE TRUE)
set_property(TARGET innative-assemblyscript-d PROPERTY VERBOSE_MAKEFILE TRUE)
  
# Directly set target compilation settings to bypass Release/Debug modes
set(CMAKE_BUILD_TYPE "")

if(MSVC)
  target_compile_options(innative-assemblyscript PRIVATE /${RUNTIME_FLAG} /WX- /GS-)
  target_compile_options(innative-assemblyscript-d PRIVATE /Zi /Od /Ob0 /${RUNTIME_FLAG}d /Gd /Gm- /GS-)
else()
  target_compile_options(innative-assemblyscript PRIVATE -O3 -msse -msse2 -msse3 -mmmx -m3dnow -mcx16 -fno-stack-protector ${C_WARNINGS})
  target_compile_options(innative-assemblyscript-d PRIVATE -g -fno-stack-protector ${C_WARNINGS})
endif()

target_compile_definitions(innative-assemblyscript PUBLIC NDEBUG)
target_compile_definitions(innative-assemblyscript-d PUBLIC DEBUG)
target_include_directories(innative-assemblyscript PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_include_directories(innative-assemblyscript-d PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_include_directories(innative-assemblyscript PUBLIC ${CMAKE_SOURCE_DIR}/innative-env)
target_include_directories(innative-assemblyscript-d PUBLIC ${CMAKE_SOURCE_DIR}/innative-env)
set_target_properties(innative-assemblyscript PROPERTIES DEBUG_POSTFIX "" POSITION_INDEPENDENT_CODE ON)
set_target_properties(innative-assemblyscript-d PROPERTIES DEBUG_POSTFIX "" POSITION_INDEPENDENT_CODE ON)

if(WIN32)
  # Cmake does not support linking with a static library
  set_target_properties(innative-assemblyscript PROPERTIES COMPILE_PDB_NAME "$(TargetName)") 
  set_target_properties(innative-assemblyscript-d PROPERTIES COMPILE_PDB_NAME "$(TargetName)") 
endif()
