set(LLVM_LINK_COMPONENTS support)

add_clang_executable(omia
        omia.cpp
        )

target_link_libraries(omia
        PRIVATE
        clangAST
        clangASTMatchers
        clangBasic
        clangFrontend
        clangSerialization
        clangTooling
        )