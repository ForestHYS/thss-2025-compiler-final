#pragma once

#include "ASTVisitor.h"
#include "SymbolTable.h"
#include "Nodes.h"
#include <memory>

namespace sysy {

/**
 * 语义分析器
 * 遍历AST，构建符号表，进行类型检查和语义分析
 */
class SemanticAnalyzer : public ASTVisitor {
private:
    SymbolTableManager* symbolTableManager;
    
    // 辅助方法
    void checkType(ExpNode* exp, DataType expectedType);
    int evaluateConstExp(ConstExpNode* node);
    
public:
    SemanticAnalyzer(SymbolTableManager* symTabMgr);
    ~SemanticAnalyzer();
    
    // 执行语义分析
    void analyze(CompUnitNode* root);
    
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

