cmake_minimum_required(VERSION 3.13.4)
project(innative-loader LANGUAGES CXX VERSION ${PROJECT_VERSION})

file(GLOB_RECURSE loader_SOURCES "./*.c")

if(WIN32)
  list(APPEND loader_SOURCES "innative-loader.rc")
else()
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
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -msse -msse2 -msse3 -mmmx -m3dnow -mcx16 -DNDEBUG")
  set(CMAKE_CXX_FLAGS_MINSIZEREL ${CMAKE_CXX_FLAGS_RELEASE})
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO ${CMAKE_CXX_FLAGS_RELEASE})
endif()

# add library and include directories
add_executable(innative-loader ${loader_SOURCES})
target_include_directories(innative-loader PUBLIC ${CMAKE_SOURCE_DIR}/include)
set_target_properties(innative-loader PROPERTIES OUTPUT_NAME_DEBUG "innative-loader-d")
if(BUILD_SHARED_LIBS)
target_link_libraries(innative-loader innative-stub)
else()
target_link_libraries(innative-loader innative)
endif()

set_target_properties(innative-loader
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
    set_target_properties(innative-loader PROPERTIES COMPILE_PDB_NAME "$(TargetName)") 
    set_target_properties(innative-loader PROPERTIES OUTPUT_NAME "innative-loader-s")
    set_target_properties(innative-loader PROPERTIES OUTPUT_NAME_DEBUG "innative-loader-s-d")
  endif()
  
  set_source_files_properties("innative-loader.rc" PROPERTIES LANGUAGE RC)
else()
  target_link_libraries(innative-loader ${LIB_RT} ${CMAKE_DL_LIBS})
endif()
