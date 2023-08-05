set(LLVM_LINK_COMPONENTS support)

add_clang_executable(owpfv
        owpfv.cpp
        )

target_link_libraries(owpfv
        PRIVATE
        clangAST
        clangASTMatchers
        clangBasic
        clangFrontend
        clangSerialization
        clangTooling
        )