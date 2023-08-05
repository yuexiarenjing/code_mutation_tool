set(LLVM_LINK_COMPONENTS support)

add_clang_executable(omvae
        omvae.cpp
        )

target_link_libraries(omvae
        PRIVATE
        clangAST
        clangASTMatchers
        clangBasic
        clangFrontend
        clangSerialization
        clangTooling
        )