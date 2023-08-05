/// This program injects MVIA and MVAV fault 
///   by editing program at AST level and write out the changed source code.
/// Usage: omviav <inFile> -d <outDir> [-f <fListFile>] --

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ParentMapContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"
#include <vector>
#include <set>
#include <cstdio>
#include <cstring>
#include <dirent.h>
#include <sys/stat.h>
#include <fstream>
#include <string>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;
using namespace llvm;

// Define options
static cl::OptionCategory ToolCategory("tool options");
static cl::opt<std::string> OutDirname("d", cl::desc("Specify output directory"), 
      cl::value_desc("directory"), cl::cat(ToolCategory));
static cl::opt<std::string> TargetFunction("f", cl::desc("Specify targeted function"),
      cl::value_desc("function"), cl::cat(ToolCategory));

// vector of source range to be removed
static std::vector<SourceRange> vDelRange;

// Visitor for injecting miss function call (MFC) fault
class RemoveVoidFunctionCallsVisitor 
    : public RecursiveASTVisitor<RemoveVoidFunctionCallsVisitor> {
public:
  RemoveVoidFunctionCallsVisitor(Rewriter &Rewrite, ASTContext &AstCtx) 
    : Rewrite(Rewrite), AstCtx(AstCtx) {}

  bool VisitFunctionDecl(FunctionDecl *FD) {
    // interested file not empty and is interestedfunction
    if (TargetFunction != "" && TargetFunction != FD->getNameAsString()) {
      return true;
    }

    Stmt *S = FD->getBody();
    if (!S) {
      return true;
    }

    exeFI(S);
    return true;
  }

private:
  Rewriter &Rewrite;
  ASTContext &AstCtx;
  std::set<std::string> FunctionList;

  bool isBasicBlockOnlyStmt(Stmt *S, ASTContext &AstCtx) {
    const Stmt *Parent = AstCtx.getParents(*S).begin()->get<Stmt>();
    if (!Parent)
      return false;

    if (isa<CompoundStmt>(Parent) && cast<CompoundStmt>(Parent)->size() == 1) {
      // Only stmt in BB
      return true;
    }

    if (isa<IfStmt>(Parent) || isa<SwitchStmt>(Parent) || isa<ForStmt>(Parent)
      || isa<WhileStmt>(Parent) || isa<DoStmt>(Parent)) {
      // Only stmt in BB or in for construct
      return true;
    }

    if (isa<BinaryOperator>(Parent)) {
      const Stmt *ParentofB = AstCtx.getParents(*Parent).begin()->get<Stmt>();
      if (isa<ForStmt>(ParentofB)) {
        return true;
      }
    }

    return false; 
  }

  void exeFI(Stmt *S) {
    if (!S) {
      return;
    }

    if (isa<BinaryOperator>(S)) {
      BinaryOperator *BO = cast<BinaryOperator>(S);
      if (BO->isAssignmentOp() || BO->isCompoundAssignmentOp()) {
        processBinaryOperator(BO);
      }   
    }

    // Recursively process the children of the statement.
    for (auto &Child : S->children()) {
      exeFI(Child);
    }
  }

  bool isLiteralAssign(Stmt* S) {
    if(!S) {
      return false;
    }

    if (isa<IntegerLiteral>(S) || isa<FixedPointLiteral>(S) 
      || isa<CharacterLiteral>(S) || isa<FloatingLiteral>(S)
      || isa<ImaginaryLiteral>(S) || isa<clang::StringLiteral>(S)) {
      return true;
    }

    if(CastExpr* castE = dyn_cast<CastExpr>(S)) {
      for (auto child : castE->children()) {
        if ((dyn_cast<IntegerLiteral>(child)) || (dyn_cast<FixedPointLiteral>(child))
          || (dyn_cast<CharacterLiteral>(child)) || (dyn_cast<FloatingLiteral>(child))
          || (dyn_cast<ImaginaryLiteral>(child)) || (dyn_cast<clang::StringLiteral>(child))) {
          return true;
        }  
      }
    }

    return false;
  }

  void processBinaryOperator(BinaryOperator *BO) {

    if (isBasicBlockOnlyStmt(BO, AstCtx)) {
      return;
    }

    if (isLiteralAssign(BO->getRHS())) {
      return;
    }

    SourceManager &SM = AstCtx.getSourceManager();
    SourceLocation StartLoc = BO->getBeginLoc();
    SourceLocation EndLoc = Lexer::findLocationAfterToken(
      BO->getEndLoc(), tok::semi, SM, AstCtx.getLangOpts(), false); // true will delete next line header for macro
  
    if (StartLoc.isValid() && EndLoc.isValid() 
        && SM.getFilename(StartLoc) != "" && SM.getFilename(EndLoc) != ""
        && SM.getFilename(StartLoc) == SM.getFileEntryForID(SM.getMainFileID())->getName()) {
      
      SourceRange DelRange = SourceRange (StartLoc, EndLoc);
    
      auto it = std::find(vDelRange.begin(), vDelRange.end(), DelRange);
      if (it == vDelRange.end()) {
        vDelRange.push_back(DelRange);
      }
    }
    return;
  }
};

// Consumer for writing out the edited source code
class RemoveVoidFunctionCallsConsumer : public ASTConsumer {
public:
  RemoveVoidFunctionCallsConsumer(CompilerInstance &CI) 
  : Rewrite(CI.getSourceManager(), CI.getLangOpts()), AstCtx(CI.getASTContext())
  , inputFile(llvm::sys::path::filename(CI.getFrontendOpts().Inputs[0].getFile()).str()) {}

  void HandleTranslationUnit(ASTContext &Context) override {
    Rewrite.setSourceMgr(Context.getSourceManager(), Context.getLangOpts());
    RemoveVoidFunctionCallsVisitor Visitor(Rewrite, AstCtx);

    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
    if (OutDirname != "") {
      writeMutateFiles(Rewrite, Context); // clone Rewrite class
    } else {
      // Rewriter::RewriteOptions opts = Rewriter::RewriteOptions();
      // opts.RemoveLineIfEmpty = true;
      for (long unsigned int i = 0; i < vDelRange.size(); i++) {
        SourceRange DelRange = vDelRange[i];
        Rewrite.RemoveText(DelRange);
      }
      Rewrite.getEditBuffer(Context.getSourceManager().getMainFileID()).write(llvm::outs());
    }
  }

private:
  Rewriter Rewrite;
  ASTContext &AstCtx;
  std::string inputFile;

  void removeDir(const char* dirPath) {
    DIR* dir = opendir(dirPath);
    if (dir == nullptr) {
      return;
    }

    dirent* entry;
    while ((entry = readdir(dir)) != nullptr) {
      if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
        continue;
      }

      std::string path = std::string(dirPath) + "/" + std::string(entry->d_name);
      if (entry->d_type == DT_DIR) {
        removeDir(path.c_str());
      } else {
        std::remove(path.c_str());
      }
    }

    closedir(dir);
    std::remove(dirPath);
  }

  void writeMutateFiles(Rewriter Rewrite, ASTContext &Context) {
    // prepare mutate dir
    const char* folder = OutDirname.c_str();
    removeDir(folder);
      mkdir(folder, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);

    for (long unsigned int i = 0; i < vDelRange.size(); i++) {
      // source range to be removed
      SourceRange DelRange = vDelRange[i];

      // mutate file name
      std::string mutateFile = std::string(folder) + "/" + inputFile + "-" + std::to_string(i);

      mutate(mutateFile, Rewrite, Context, DelRange);

      // SourceManager &SM = AstCtx.getSourceManager();
      // const auto& mainFile = SM.getFileEntryForID(SM.getMainFileID());
      // llvm::errs() << "@" << mainFile->getName()<< "\n";
      // if (mainFile->getName() == SM.getFilename(DelRange.getBegin())) {
      //   llvm::errs() << "@1" << "\n";
      // }
      // unsigned line = SM.getSpellingLineNumber(DelRange.getBegin());
      // unsigned col = SM.getSpellingColumnNumber(DelRange.getBegin());
      // llvm::errs() << line << ":" << col << " " << SM.getFilename(DelRange.getBegin()) << "\n";
      // line = SM.getSpellingLineNumber(DelRange.getEnd());
      // col = SM.getSpellingColumnNumber(DelRange.getEnd());
      // llvm::errs() << line << ":" << col << " " << SM.getFilename(DelRange.getEnd()) << "\n";
      // llvm::errs() << "#\n";
    }
  }

  void mutate(std::string OutputFileName, Rewriter Rewrite
        , ASTContext &Context, SourceRange DelRange) {

    Rewrite.RemoveText(DelRange);
    std::error_code EC;
    llvm::raw_fd_ostream OS(OutputFileName, EC, llvm::sys::fs::OF_None);
    if (EC) {
      llvm::errs() << "Error opening output file: " << EC.message() << "\n";
      return;
    }
    Rewrite.getEditBuffer(Context.getSourceManager().getMainFileID()).write(OS);
  }
};

// For each source file provided to the tool, a new FrontendAction is created.
class MyFrontendAction : public ASTFrontendAction {
public:
  MyFrontendAction() {}

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
    StringRef file) override {
    llvm::errs() << "** Creating AST consumer for: " << file << "\n";

    return std::make_unique<RemoveVoidFunctionCallsConsumer>(CI);
  }
};

int main(int argc, const char **argv) {
  auto ExpectedParser = CommonOptionsParser::create(argc, argv, ToolCategory);
  if (!ExpectedParser) {
    // Fail gracefully for unsupported options.
    llvm::errs() << ExpectedParser.takeError();
    return 1;
  }
  CommonOptionsParser& OptionsParser = ExpectedParser.get();
  ClangTool Tool(OptionsParser.getCompilations(), OptionsParser.getSourcePathList());

  return Tool.run(newFrontendActionFactory<MyFrontendAction>().get());
}

