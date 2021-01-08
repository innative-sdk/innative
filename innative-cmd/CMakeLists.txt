cmake_minimum_required(VERSION 3.13.4)
project(innative-cmd LANGUAGES CXX VERSION ${PROJECT_VERSION})

option(BUILD_SHARED_LIBS "enable shared library" ON)
option(DYNAMIC_RUNTIME "if true, dynamically links (/MD) to the C++ runtime on MSVC. Otherwise, statically links (/MT)" OFF)

if(USE32bit)
  set(BIN_DIR "bin-Win32")
else()
  set(BIN_DIR "bin")
endif()

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
  find_library(LIB_RT REQUIRED)
endif()

set(CMAKE_VERBOSE_MAKEFILE FALSE)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_POSITION_INDEPENDENT_CODE OFF)

if(MSVC)
  set(RUNTIME_FLAG "MT")
  if(DYNAMIC_RUNTIME)
    set(RUNTIME_FLAG "MD")
  endif()

  string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /${RUNTIME_FLAG}d")
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /Oi /Ot /GL /${RUNTIME_FLAG}")
  set(CMAKE_CXX_FLAGS_MINSIZEREL ${CMAKE_CXX_FLAGS_RELEASE})
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO ${CMAKE_CXX_FLAGS_RELEASE})
else()
  string(REGEX REPLACE "-fexceptions" "" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-exceptions")
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -DDEBUG")
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -msse -msse2 -msse3 -mmmx -m3dnow -mcx1 -DNDEBUG")
  set(CMAKE_CXX_FLAGS_MINSIZEREL ${CMAKE_CXX_FLAGS_RELEASE})
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO ${CMAKE_CXX_FLAGS_RELEASE})
endif()

# add library and include directories
add_executable(innative-cmd ${cmd_SOURCES})
target_include_directories(innative-cmd PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_target_properties(innative-cmd PROPERTIES OUTPUT_NAME_DEBUG "innative-cmd-d")
target_link_libraries(innative-cmd innative)

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