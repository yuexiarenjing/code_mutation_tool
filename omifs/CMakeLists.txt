set(LLVM_LINK_COMPONENTS support)

add_clang_executable(omifs
        omifs.cpp
        )

target_link_libraries(omifs
        PRIVATE
        clangAST
        clangASTMatchers
        clangBasic
        clangFrontend
        clangSerialization
        clangTooling
        )