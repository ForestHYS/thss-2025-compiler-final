#pragma once

#include "ASTVisitor.h"
#include "SymbolTable.h"
#include "Nodes.h"  // 包含完整定义
#include <memory>
#include <string>
#include <sstream>

namespace sysy {

/**
 * LLVM IR代码生成器
 * 继承ASTVisitor，遍历AST并生成LLVM IR文本代码
 * 不使用LLVM库，直接生成文本格式的IR
 */
class CodeGenerator : public ASTVisitor {
private:
    SymbolTableManager* symbolTableManager;
    std::ostringstream irStream;  // 用于生成IR文本
    
    // 控制流相关（用于生成标签）
    std::string currentBreakTarget;
    std::string currentContinueTarget;
    int labelCounter;  // 用于生成唯一的标签名
    
    // 辅助方法
    std::string getLLVMType(DataType dataType);
    std::string getLLVMArrayType(DataType elementType, const std::vector<int>& dimensions);
    std::string generateLabel(const std::string& prefix);
    std::string generateVarName(const std::string& name);
    
public:
    CodeGenerator(SymbolTableManager* symTabMgr);
    ~CodeGenerator();
    
    // 生成代码的主入口，返回IR文本
    std::string generate(CompUnitNode* root);
    
    // 编译单元
    void visitCompUnit(CompUnitNode* node) override;
    
    // 声明
    void visitConstDecl(ConstDeclNode* node) override;
    void visitVarDecl(VarDeclNode* node) override;
    void visitConstDef(ConstDefNode* node) override;
    void visitVarDef(VarDefNode* node) override;
    void visitConstInitVal(ConstInitValNode* node) override;
    void visitInitVal(InitValNode* node) override;
    
    // 函数
    void visitFuncDef(FuncDefNode* node) override;
    void visitFuncFParam(FuncFParamNode* node) override;
    
    // 语句
    void visitBlockStmt(BlockStmtNode* node) override;
    void visitBlockItem(BlockItemNode* node) override;
    void visitAssignStmt(AssignStmtNode* node) override;
    void visitExpStmt(ExpStmtNode* node) override;
    void visitIfStmt(IfStmtNode* node) override;
    void visitWhileStmt(WhileStmtNode* node) override;
    void visitBreakStmt(BreakStmtNode* node) override;
    void visitContinueStmt(ContinueStmtNode* node) override;
    void visitReturnStmt(ReturnStmtNode* node) override;
    
    // 表达式
    void visitAddExp(AddExpNode* node) override;
    void visitMulExp(MulExpNode* node) override;
    void visitUnaryExp(UnaryExpNode* node) override;
    void visitPrimaryExp(PrimaryExpNode* node) override;
    void visitLVal(LValNode* node) override;
    void visitNumber(NumberNode* node) override;
    void visitFuncCall(FuncCallNode* node) override;
    void visitRelExp(RelExpNode* node) override;
    void visitEqExp(EqExpNode* node) override;
    void visitLAndExp(LAndExpNode* node) override;
    void visitLOrExp(LOrExpNode* node) override;
    void visitCond(CondNode* node) override;
    void visitConstExp(ConstExpNode* node) override;
};

} // namespace sysy

