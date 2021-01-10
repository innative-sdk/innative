cmake_minimum_required(VERSION 3.13.4)
project(innative-test-embedding LANGUAGES C VERSION ${PROJECT_VERSION})

set(CMAKE_VERBOSE_MAKEFILE FALSE)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

file(GLOB_RECURSE testembed_SOURCES "./*.c")

if(MSVC)
  string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_C_FLAGS ${CMAKE_C_FLAGS})
  string(REGEX REPLACE "/RTC1" "" CMAKE_C_FLAGS_DEBUG ${CMAKE_C_FLAGS_DEBUG})
else()
  string(REGEX REPLACE "-fexceptions" "" CMAKE_C_FLAGS ${CMAKE_C_FLAGS})
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-exceptions")
endif()

# add library and include directories
add_library(innative-test-embedding STATIC ${testembed_SOURCES})
add_library(innative-test-embedding-d STATIC ${testembed_SOURCES})

# Directly set target compilation settings to bypass Release/Debug modes
set(CMAKE_BUILD_TYPE "")

if(MSVC)
  target_compile_options(innative-test-embedding PRIVATE /D "NDEBUG" /${RUNTIME_FLAG} /WX- /GS-)
  target_compile_options(innative-test-embedding-d PRIVATE /D "DEBUG" /Zi /Od /Ob0 /${RUNTIME_FLAG}d /Gd /Gm- /GS-)
else()
  target_compile_options(innative-test-embedding PRIVATE -fno-stack-protector -DNDEBUG -O3)
  target_compile_options(innative-test-embedding-d PRIVATE -fno-stack-protector -DDEBUG -g)
endif()

target_include_directories(innative-test-embedding PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_include_directories(innative-test-embedding-d PUBLIC ${CMAKE_SOURCE_DIR}/include)

set_target_properties(innative-test-embedding
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

set_target_properties(innative-test-embedding-d
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
  set_target_properties(innative-test-embedding PROPERTIES COMPILE_PDB_NAME "$(TargetName)")
  set_target_properties(innative-test-embedding-d PROPERTIES COMPILE_PDB_NAME "$(TargetName)") 
endif()