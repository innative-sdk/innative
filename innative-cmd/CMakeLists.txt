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
else()
  string(REPLACE "-fexceptions" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
endif()

# add library and include directories
add_executable(innative-cmd ${cmd_SOURCES})
target_include_directories(innative-cmd PUBLIC ${CMAKE_SOURCE_DIR}/include)

if(MSVC)
  target_link_libraries(innative-cmd innative)
else()
  target_link_libraries(innative-cmd innative stdc++fs)
endif()

standard_target_flags(innative-cmd)
retarget_output(innative-cmd)

if(WIN32)
  # Cmake does not support linking with a static library
  if(NOT BUILD_SHARED_LIBS)
    set_target_properties(innative-cmd PROPERTIES COMPILE_PDB_NAME "$(TargetName)") 
    set_target_properties(innative-cmd PROPERTIES OUTPUT_NAME "innative-cmd-s")
  endif()
  
  set_source_files_properties("innative-cmd.rc" PROPERTIES LANGUAGE RC)
else()
  target_link_libraries(innative-cmd ${LIB_RT} ${ZLIB} ${Threads} ${CMAKE_DL_LIBS})
endif()
