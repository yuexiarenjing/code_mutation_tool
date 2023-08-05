set(LLVM_LINK_COMPONENTS support)

add_clang_executable(owaep
        owaep.cpp
        )

target_link_libraries(owaep
        PRIVATE
        clangAST
        clangASTMatchers
        clangBasic
        clangFrontend
        clangSerialization
        clangTooling
        )