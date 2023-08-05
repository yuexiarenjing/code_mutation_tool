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
#include <cstdint>

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

// Text to be replaced
// Note: do not use <StringRef>, leads to strange bug
static std::vector<std::string> vRepText;

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

    if (isa<CallExpr>(S)) {
      CallExpr *CE = cast<CallExpr>(S);
      ProcessCallExpr(CE);
    }

    // Recursively process the children of the statement.
    for (auto &Child : S->children()) {
      exeFI(Child);
    }
  }

  void doReplace(SourceLocation StartLoc, SourceLocation EndLoc, std::string new_code) {
    SourceManager &SM = AstCtx.getSourceManager();

    if (StartLoc.isValid() && EndLoc.isValid() 
      && SM.getFilename(StartLoc) != "" && SM.getFilename(EndLoc) != ""
      && SM.getFilename(StartLoc) == SM.getFileEntryForID(SM.getMainFileID())->getName()) {

      SourceRange RepRange = SourceRange(StartLoc, EndLoc);

      auto it = std::find(vDelRange.begin(), vDelRange.end(), RepRange);
      if (it == vDelRange.end()) {
        vDelRange.push_back(RepRange);
        vRepText.push_back(new_code);
      }
    }
  }

  void DoubleToString(double dNum,char *pStr)
  {
      uint64_t nData = ((uint64_t *)&dNum)[0];
   
      for (int i = 0;i < 64;i ++)
      {
          pStr[63 - i] = (char)(nData & 1) + '0';
          if (i >= 48 && i <= 55)
          {
              if (pStr[63 - i] == '0')
                  pStr[63 - i]= '1';
              else
                  pStr[63 - i]= '0';

          }
          nData >>= 1;
      }
      pStr[64] = '\0';
  }


  double StringToDouble(char *pStr)
  {
      uint64_t nData = 0;
      double *pData;
   
      for (int i = 0;i < 63;i ++)
      {
          nData += (pStr[i] - '0');
          nData <<= 1;
      }
      nData += (pStr[63] - '0');
      pData = (double *)&nData;
      return *pData;
  }

  void repIntergerChild(Stmt* S) {
    if (!S) {
      return;
    }

    if (IntegerLiteral* IL = dyn_cast<IntegerLiteral>(S)) {
      uint64_t val = IL->getValue().getZExtValue();
      uint64_t new_val = val ^ 0xff;
      std::string new_code = std::to_string(new_val);

      doReplace(IL->getBeginLoc(), IL->getEndLoc(), new_code);
    }

    if (FloatingLiteral* FL = dyn_cast<FloatingLiteral>(S)) {
      APFloat F = FL->getValue();
      double d = F.convertToDouble();
      char s[64];
      DoubleToString(d, s);
      d = StringToDouble(s);
      std::string new_code = std::to_string(d);

      doReplace(FL->getBeginLoc(), FL->getEndLoc(), new_code);
    }

    for (auto &Child : S->children()) {
      repIntergerChild(Child);
    }

    return; 
  }

  bool notInUnaryOperator(Stmt* S) {
    const Stmt *Parent = AstCtx.getParents(*S).begin()->get<Stmt>();

    if (isa<UnaryOperator>(Parent)) {
      return true;
    }

    return false;
  }

  void repDeclRefExprChild(Stmt* S) {
    if (!S) {
      return;
    }

    if (DeclRefExpr* DRE = dyn_cast<DeclRefExpr>(S)) {
      if (DRE->getType()->isIntegerType() && !notInUnaryOperator(DRE)) {

        std::string name = DRE->getNameInfo().getAsString();
        name += " ^ 0xff";

        doReplace(DRE->getBeginLoc(), DRE->getEndLoc(), name);
      }

    }

    for (auto &Child : S->children()) {
      repDeclRefExprChild(Child);
    }

    return; 
  }

  void ProcessCallExpr(CallExpr *CE) {
    for (unsigned i = 0, e = CE->getNumArgs(); i != e; ++i) {
      repIntergerChild(CE->getArg(i));
      repDeclRefExprChild(CE->getArg(i));
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
        Rewrite.ReplaceText(vDelRange[i], StringRef(vRepText[i]));
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
      StringRef new_code = vRepText[i];

      // mutate file name
      std::string mutateFile = std::string(folder) + "/" + inputFile + "-" + std::to_string(i);

      mutate(mutateFile, Rewrite, Context, DelRange, new_code);

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
        , ASTContext &Context, SourceRange DelRange, StringRef new_code) {

    Rewrite.ReplaceText(DelRange, new_code);
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
