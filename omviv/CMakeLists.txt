set(LLVM_LINK_COMPONENTS support)

add_clang_executable(omviv
        omviv.cpp
        )

target_link_libraries(omviv
        PRIVATE
        clangAST
        clangASTMatchers
        clangBasic
        clangFrontend
        clangSerialization
        clangTooling
        )