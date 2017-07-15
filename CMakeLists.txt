cmake_minimum_required (VERSION 2.8)
project (parser)

option(LLVM_BUILD_EXAMPLES FALSE)
option(LLVM_INCLUDE_EXAMPLES FALSE)
option(LLVM_BUILD_TESTS FALSE)
option(LLVM_INCLUDE_TESTS FALSE)
option(LLVM_BUILD_DOCS FALSE)

option(CLANG_BUILD_EXAMPLES FALSE)
option(CLANG_INCLUDE_EXAMPLES FALSE)
option(CLANG_BUILD_TESTS FALSE)
option(CLANG_INCLUDE_TESTS FALSE)
option(CLANG_BUILD_DOCS FALSE)

set(LLVM_TARGETS_TO_BUILD "X86")
set(LLVM_EXTERNAL_CLANG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/clang)

MESSAGE( STATUS "$LLVM_EXTERNAL_CLANG_SOURCE_DIR:         " ${LLVM_EXTERNAL_CLANG_SOURCE_DIR} )

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/llvm)

include_directories(${LLVM_INCLUDE_DIRS})
include_directories(${CLANG_INCLUDE_DIRS})

add_definitions(${LLVM_DEFINITIONS})
add_definitions(${CLANG_DEFINITIONS})

include_directories("${LLVM_EXTERNAL_CLANG_SOURCE_DIR}/include")
include_directories("llvm/include")
include_directories("${CMAKE_CURRENT_BINARY_DIR}/llvm/include")
include_directories("${CMAKE_CURRENT_BINARY_DIR}/llvm/tools/clang/include")

add_executable(parser "parser.cpp")

llvm_map_components_to_libnames(llvm_libs support core irreader MCJIT)

set(CLANG_LIBS clangFrontend  clangSerialization clangDriver clangParse clangRewriteFrontend clangStaticAnalyzerFrontend clangSema)
set(CLANG_LIBS ${CLANG_LIBS} clangAnalysis clangEdit clangAST clangLex clangBasic clangTooling )


MESSAGE(STATUS "Clang libs: " ${CLANG_LIBS})

target_link_libraries(parser ${llvm_libs} ${CLANG_LIBS})

