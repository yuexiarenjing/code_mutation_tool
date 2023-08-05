set(LLVM_LINK_COMPONENTS support)

add_clang_executable(omvav
        omvav.cpp
        )

target_link_libraries(omvav
        PRIVATE
        clangAST
        clangASTMatchers
        clangBasic
        clangFrontend
        clangSerialization
        clangTooling
        )