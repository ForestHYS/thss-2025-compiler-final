#pragma once

#include "ASTBuilder.h"
#include "SemanticAnalyzer.h"
#include "CodeGenerator.h"
#include "SymbolTable.h"
#include <memory>
#include <string>

namespace sysy {

/**
 * 编译器主类
 * 协调各个编译阶段：词法分析、语法分析、AST构建、语义分析、代码生成
 */
class Compiler {
private:
    std::unique_ptr<ASTBuilder> astBuilder;
    std::unique_ptr<SymbolTableManager> symbolTableManager;
    std::unique_ptr<SemanticAnalyzer> semanticAnalyzer;
    std::unique_ptr<CodeGenerator> codeGenerator;
    
    // 编译阶段
    std::unique_ptr<CompUnitNode> parse(const std::string& inputFile);
    void semanticAnalysis(CompUnitNode* ast);
    std::string generateCode(CompUnitNode* ast);
    void outputIR(const std::string& outputFile, const std::string& irCode);
    
public:
    Compiler();
    ~Compiler();
    
    /**
     * 编译主入口
     * @param inputFile 输入文件路径
     * @param outputFile 输出文件路径
     * @return 是否编译成功
     */
    bool compile(const std::string& inputFile, const std::string& outputFile);
};

} // namespace sysy

