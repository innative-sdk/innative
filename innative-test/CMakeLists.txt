cmake_minimum_required(VERSION 3.13.4)
project(innative-test LANGUAGES CXX VERSION ${PROJECT_VERSION})

file(GLOB_RECURSE test_SOURCES "./*.cpp")

if(WIN32)
  list(APPEND test_SOURCES "innative-test.rc")
else()
  find_package(Threads REQUIRED)
  find_library(LIB_RT REQUIRED)
endif()

set(CMAKE_VERBOSE_MAKEFILE FALSE)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_POSITION_INDEPENDENT_CODE OFF)

if(MSVC)
  string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /${RUNTIME_FLAG}d /D \"TESTING_WASM\"")
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /Oi /Ot /GL /${RUNTIME_FLAG} /D \"TESTING_WASM\"")
  set(CMAKE_CXX_FLAGS_MINSIZEREL ${CMAKE_CXX_FLAGS_RELEASE})
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO ${CMAKE_CXX_FLAGS_RELEASE})
else()
  string(REGEX REPLACE "-fexceptions" "" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-exceptions")
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -DDEBUG -DTESTING_WASM -DLIBICONV_PLUG")
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -msse -msse2 -msse3 -mmmx -m3dnow -mcx16 -DNDEBUG -DTESTING_WASM -DLIBICONV_PLUG")
  set(CMAKE_CXX_FLAGS_MINSIZEREL ${CMAKE_CXX_FLAGS_RELEASE})
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO ${CMAKE_CXX_FLAGS_RELEASE})
endif()

# add library and include directories
add_executable(innative-test ${test_SOURCES} ${CMAKE_SOURCE_DIR}/scripts/wasm_malloc.c)
target_include_directories(innative-test PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_target_properties(innative-test PROPERTIES OUTPUT_NAME_DEBUG "innative-test-d")
target_link_libraries(innative-test innative innative-env)

set_target_properties(innative-test
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
  if(NOT BUILD_SHARED_LIBS)
    set_target_properties(innative-test PROPERTIES COMPILE_PDB_NAME "$(TargetName)") 
    set_target_properties(innative-test PROPERTIES OUTPUT_NAME "innative-test-s")
    set_target_properties(innative-test PROPERTIES OUTPUT_NAME_DEBUG "innative-test-s-d")
  endif()
  
  set_source_files_properties("innative-test.rc" PROPERTIES LANGUAGE RC)
else()
  target_link_libraries(innative-test ${LIB_RT} ${Threads} ${CMAKE_DL_LIBS})
endif()