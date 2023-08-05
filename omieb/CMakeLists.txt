set(LLVM_LINK_COMPONENTS support)

add_clang_executable(omieb
        omieb.cpp
        )

target_link_libraries(omieb
        PRIVATE
        clangAST
        clangASTMatchers
        clangBasic
        clangFrontend
        clangSerialization
        clangTooling
        )