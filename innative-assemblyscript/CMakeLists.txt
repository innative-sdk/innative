cmake_minimum_required(VERSION 3.13.4)
project(innative-assemblyscript LANGUAGES C VERSION ${PROJECT_VERSION})

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

file(GLOB_RECURSE assemblyscript_SOURCES "./*.c")

if(MSVC)
  string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
  string(REPLACE "/RTC1" "" CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
else()
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${CPP_WARNINGS} -fno-stack-protector")
endif()

# add library and include directories
add_library(innative-assemblyscript STATIC ${assemblyscript_SOURCES})
add_library(innative-assemblyscript-d STATIC ${assemblyscript_SOURCES})

# Directly set target compilation settings to bypass Release/Debug modes
set(CMAKE_BUILD_TYPE "")

if(MSVC)
  target_compile_options(innative-assemblyscript PRIVATE /D "NDEBUG" /${RUNTIME_FLAG} /WX- /GS-)
  target_compile_options(innative-assemblyscript-d PRIVATE /D "DEBUG" /Zi /Od /Ob0 /${RUNTIME_FLAG}d /Gd /Gm- /GS-)
else()
  target_compile_options(innative-assemblyscript PRIVATE -DNDEBUG -O3 -msse -msse2 -msse3 -mmmx -m3dnow -mcx16)
  target_compile_options(innative-assemblyscript-d PRIVATE -DDEBUG -g)
endif()

target_include_directories(innative-assemblyscript PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_include_directories(innative-assemblyscript-d PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_include_directories(innative-assemblyscript PUBLIC ${CMAKE_SOURCE_DIR}/innative-env)
target_include_directories(innative-assemblyscript-d PUBLIC ${CMAKE_SOURCE_DIR}/innative-env)

set_target_properties(innative-assemblyscript
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
    PREFIX ""
)

set_target_properties(innative-assemblyscript-d
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
    PREFIX ""
)

if(WIN32)
  # Cmake does not support linking with a static library
  set_target_properties(innative-assemblyscript PROPERTIES COMPILE_PDB_NAME "$(TargetName)") 
  set_target_properties(innative-assemblyscript-d PROPERTIES COMPILE_PDB_NAME "$(TargetName)") 
endif()
