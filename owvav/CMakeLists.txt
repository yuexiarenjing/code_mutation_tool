set(LLVM_LINK_COMPONENTS support)

add_clang_executable(owvav
        owvav.cpp
        )

target_link_libraries(owvav
        PRIVATE
        clangAST
        clangASTMatchers
        clangBasic
        clangFrontend
        clangSerialization
        clangTooling
        )