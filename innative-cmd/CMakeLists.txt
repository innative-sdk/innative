cmake_minimum_required(VERSION 3.13.4)
project(innative-cmd LANGUAGES CXX VERSION ${PROJECT_VERSION})

include(CheckSymbolExists)
include(CMakePushCheckState)

file(GLOB_RECURSE cmd_SOURCES "./*.cpp")

if(WIN32)
  list(APPEND cmd_SOURCES "innative-cmd.rc")
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

set(CMAKE_POSITION_INDEPENDENT_CODE OFF)

if(MSVC)
  string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /${RUNTIME_FLAG}d")
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /Oi /Ot /GL /${RUNTIME_FLAG}")
  set(CMAKE_CXX_FLAGS_MINSIZEREL ${CMAKE_CXX_FLAGS_RELEASE})
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO ${CMAKE_CXX_FLAGS_RELEASE})
else()
  string(REPLACE "-fexceptions" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-exceptions ${CPP_WARNINGS} -Wno-reorder")
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -DDEBUG")
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -msse -msse2 -msse3 -mmmx -m3dnow -mcx1 -DNDEBUG")
  set(CMAKE_CXX_FLAGS_MINSIZEREL ${CMAKE_CXX_FLAGS_RELEASE})
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO ${CMAKE_CXX_FLAGS_RELEASE})
endif()

# add library and include directories
add_executable(innative-cmd ${cmd_SOURCES})
target_include_directories(innative-cmd PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_target_properties(innative-cmd PROPERTIES OUTPUT_NAME_DEBUG "innative-cmd-d")
target_link_libraries(innative-cmd innative stdc++fs)

set_target_properties(innative-cmd
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
    set_target_properties(innative-cmd PROPERTIES COMPILE_PDB_NAME "$(TargetName)") 
    set_target_properties(innative-cmd PROPERTIES OUTPUT_NAME "innative-cmd-s")
    set_target_properties(innative-cmd PROPERTIES OUTPUT_NAME_DEBUG "innative-cmd-s-d")
  endif()
  
  set_source_files_properties("innative-cmd.rc" PROPERTIES LANGUAGE RC)
else()
  target_link_libraries(innative-cmd ${LIB_RT} ${ZLIB} ${Threads} ${CMAKE_DL_LIBS})
endif()
