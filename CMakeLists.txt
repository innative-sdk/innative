cmake_minimum_required(VERSION 3.4.3)
project(inNative)

SET(CMAKE_PREFIX_PATH  "bin/llvm;${CMAKE_PREFIX_PATH}")
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

file(GLOB_RECURSE inNative_SOURCES "innative/*.cpp")

SET(GCC_COVERAGE_COMPILE_FLAGS "-fPIC -std=c++14")
SET(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} ${GCC_COVERAGE_COMPILE_FLAGS}")

# Now build our tools
add_library(innative SHARED ${inNative_SOURCES})
target_include_directories(innative PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>)

# Find the libraries that correspond to the LLVM components we need
llvm_map_components_to_libnames(llvm_libs support core irreader analysis X86AsmParser X86AsmPrinter X86CodeGen)

# Link against LLVM libraries
target_link_libraries(innative ${llvm_libs})