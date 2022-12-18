cmake_minimum_required(VERSION 3.13.4)
project(innative-loader LANGUAGES C VERSION ${PROJECT_VERSION})

file(GLOB_RECURSE loader_SOURCES "./*.c")

if(WIN32)
  list(APPEND loader_SOURCES "innative-loader.rc")
else()
  find_library(LIB_RT rt REQUIRED)
endif()

if(MSVC)
  string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
else()
  string(REPLACE "-fexceptions" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
endif()

# add library and include directories
add_executable(innative-loader ${loader_SOURCES})
target_include_directories(innative-loader PUBLIC ${CMAKE_SOURCE_DIR}/include)
if(BUILD_SHARED_LIBS)
target_link_libraries(innative-loader innative-stub)
else()
target_link_libraries(innative-loader innative)
endif()

set_target_properties(innative-loader PROPERTIES COMPILE_PDB_NAME "$(TargetName)" POSITION_INDEPENDENT_CODE OFF) 
retarget_output(innative-loader)
standard_target_flags(innative-loader)

if(WIN32)
  # Cmake does not support linking with a static library
  if(NOT BUILD_SHARED_LIBS)
    set_target_properties(innative-loader PROPERTIES OUTPUT_NAME "innative-loader-s")
  endif()
  
  set_source_files_properties("innative-loader.rc" PROPERTIES LANGUAGE RC)
else()
  target_link_libraries(innative-loader ${LIB_RT} ${CMAKE_DL_LIBS})
endif()
