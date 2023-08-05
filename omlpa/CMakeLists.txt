set(LLVM_LINK_COMPONENTS support)

add_clang_executable(omlpa
        omlpa.cpp
        )

target_link_libraries(omlpa
        PRIVATE
        clangAST
        clangASTMatchers
        clangBasic
        clangFrontend
        clangSerialization
        clangTooling
        )