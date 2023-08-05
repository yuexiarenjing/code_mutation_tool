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

  void exeFI(Stmt *S) {
    if (!S) {
      return;
    }

    if (isa<IfStmt>(S)) {
      IfStmt * IS = cast<IfStmt>(S);
      processIfStmt(IS);
    }

    // Recursively process the children of the statement.
    for (auto &Child : S->children()) {
      exeFI(Child);
    }
  }

  void processIfStmt(IfStmt *IS) {
    // if no else return
    if (!IS->getElse()) {
      return;
    }

    SourceManager &SM = AstCtx.getSourceManager();
    SourceLocation StartLoc = IS->getBeginLoc();
    SourceLocation EndLoc;

    bool hasCompond = false;

    Stmt* then = IS->getThen();
    Stmt* ES = IS->getElse();

    CompoundStmt* CS;
    if ((CS = dyn_cast<CompoundStmt>(ES))) {
      hasCompond = true;
      EndLoc = CS->getBeginLoc();
    } else if (IfStmt* inIS = dyn_cast<IfStmt>(ES)) {
      EndLoc = inIS->getBeginLoc().getLocWithOffset(-1);
    } else {
      EndLoc = then->getEndLoc();
    }

    if (StartLoc.isValid() && EndLoc.isValid() 
        && SM.getFilename(StartLoc) != "" && SM.getFilename(EndLoc) != ""
        && SM.getFilename(StartLoc) == SM.getFileEntryForID(SM.getMainFileID())->getName()) {
      
      SourceRange DelRange = SourceRange (StartLoc, EndLoc);
    
      auto it = std::find(vDelRange.begin(), vDelRange.end(), DelRange);
      if (it == vDelRange.end()) {
        vDelRange.push_back(DelRange);
        if (hasCompond) {
          vDelRange.push_back(SourceRange(CS->getEndLoc(), CS->getEndLoc()));
        }
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

  bool isRBraceChar(SourceLocation Loc) {
    if (!Loc.isValid()) {
      return false;
    }

    CharSourceRange Range = Lexer::getAsCharRange(
              CharSourceRange::getTokenRange(Loc, Loc), AstCtx.getSourceManager(),
              AstCtx.getLangOpts());

    StringRef Text = Lexer::getSourceText(Range, AstCtx.getSourceManager(),
                                                AstCtx.getLangOpts());

    if (Text.startswith("}")) {
      return true;
    }

    return false;
  }

  void writeMutateFiles(Rewriter Rewrite, ASTContext &Context) {
    // prepare mutate dir
    const char* folder = OutDirname.c_str();
    removeDir(folder);
      mkdir(folder, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);

    for (long unsigned int i = 0, j = 0; i < vDelRange.size(); i++, j++) {
      // source range to be removed
      SourceRange DelRange = vDelRange[i];

      // mutate file name
      std::string mutateFile = std::string(folder) + "/" + inputFile + "-" + std::to_string(j);

      if (i+1 < vDelRange.size() && vDelRange[i+1].getBegin() == vDelRange[i+1].getEnd()
        && isRBraceChar(vDelRange[i+1].getBegin())) {

        mutate(mutateFile, Rewrite, Context, DelRange, vDelRange[i+1]);
        ++i;
      } else {
        mutate(mutateFile, Rewrite, Context, DelRange);
      }

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

  void mutate(std::string OutputFileName, Rewriter Rewrite
        , ASTContext &Context, SourceRange DelRange1, SourceRange DelRange2) {

    Rewrite.RemoveText(DelRange1);
    Rewrite.RemoveText(DelRange2);
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
