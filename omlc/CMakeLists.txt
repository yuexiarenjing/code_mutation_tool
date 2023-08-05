set(LLVM_LINK_COMPONENTS support)

add_clang_executable(omlc
        omlc.cpp
        )

target_link_libraries(omlc
        PRIVATE
        clangAST
        clangASTMatchers
        clangBasic
        clangFrontend
        clangSerialization
        clangTooling
        )