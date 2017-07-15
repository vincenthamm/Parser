cmake_minimum_required (VERSION 2.8)
project (parser)

include_directories("llvm/include")
include_directories("llvm/tools/clang/include")
include_directories("llvm_cmake/include")
include_directories("llvm_cmake/tools/clang/include")

add_executable(parser "parser.cpp")

set_target_properties(parser PROPERTIES LINK_FLAGS_RELEASE /LIBPATH:${CMAKE_CURRENT_LIST_DIR}/llvm_cmake/Release/lib/)
set_target_properties(parser PROPERTIES LINK_FLAGS_DEBUG /LIBPATH:${CMAKE_CURRENT_LIST_DIR}/llvm_cmake/Debug/lib/)


