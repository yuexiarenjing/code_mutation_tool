# Overview

This is a fault injection tool built on the top of Clang/LLVM for C/C++ programs. It performs code mutations by scanning the IR codes generated from source codes, parsing the AST of IR codes, and generating faulty versions of source codes.

# Requirements:

Clang/LLVM 13.0.0

cmake 3.18.2

GCC 4.8.5

Copy the source codes to clang/llvm project, for example:

```bash
cp -R path_of_code_mutation_tool/* /root/llvm-project-llvmorg-13.0.0/clang/tools
```

Then compile or re-compile the Clang/LLVM project, the built tools can be found in /root/llvm-project-llvmorg-13.0.0/build/bin.

# Usage example:

This program (omfc) injects MFC fault 
by editing program at AST level and write out the changed source code.

inFile: path of the target C/C++ sorce code file

outDir: output directory for mutated codes

fListFile: path of target function list file (each line one function)

```bash
omfc <inFile> -d <outDir> [-f <fListFile>] --
```

other fault injectors:

```bash
omia <inFile> -d <outDir> [-f <fListFile>] --
omieb <inFile> -d <outDir> [-f <fListFile>] --
omifs <inFile> -d <outDir> [-f <fListFile>] --
omlc <inFile> -d <outDir> [-f <fListFile>] --
omlpa <inFile> -d <outDir> [-f <fListFile>] --
omvae <inFile> -d <outDir> [-f <fListFile>] --
omvav <inFile> -d <outDir> [-f <fListFile>] --
omviv <inFile> -d <outDir> [-f <fListFile>] --
omaep <inFile> -d <outDir> [-f <fListFile>] --
owpfv <inFile> -d <outDir> [-f <fListFile>] --
owvav <inFile> -d <outDir> [-f <fListFile>] --
```
