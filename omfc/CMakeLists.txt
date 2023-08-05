set(LLVM_LINK_COMPONENTS support)

add_clang_executable(omfc
        omfc.cpp
        )

target_link_libraries(omfc
        PRIVATE
        clangAST
        clangASTMatchers
        clangBasic
        clangFrontend
        clangSerialization
        clangTooling
        )