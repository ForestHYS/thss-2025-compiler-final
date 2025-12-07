#pragma once

#include "Nodes.h"  // 包含完整定义

namespace sysy {

/**
 * AST访问者基类（访问者模式）
 * 用于遍历AST并执行各种操作（如代码生成、类型检查等）
 */
class ASTVisitor {
public:
    virtual ~ASTVisitor() = default;
    
    // 编译单元
    virtual void visitCompUnit(CompUnitNode* node) = 0;
    
    // 声明
    virtual void visitConstDecl(ConstDeclNode* node) = 0;
    virtual void visitVarDecl(VarDeclNode* node) = 0;
    virtual void visitConstDef(ConstDefNode* node) = 0;
    virtual void visitVarDef(VarDefNode* node) = 0;
    virtual void visitConstInitVal(ConstInitValNode* node) = 0;
    virtual void visitInitVal(InitValNode* node) = 0;
    
    // 函数
    virtual void visitFuncDef(FuncDefNode* node) = 0;
    virtual void visitFuncFParam(FuncFParamNode* node) = 0;
    
    // 语句
    virtual void visitBlockStmt(BlockStmtNode* node) = 0;
    virtual void visitBlockItem(BlockItemNode* node) = 0;
    virtual void visitAssignStmt(AssignStmtNode* node) = 0;
    virtual void visitExpStmt(ExpStmtNode* node) = 0;
    virtual void visitIfStmt(IfStmtNode* node) = 0;
    virtual void visitWhileStmt(WhileStmtNode* node) = 0;
    virtual void visitBreakStmt(BreakStmtNode* node) = 0;
    virtual void visitContinueStmt(ContinueStmtNode* node) = 0;
    virtual void visitReturnStmt(ReturnStmtNode* node) = 0;
    
    // 表达式
    virtual void visitAddExp(AddExpNode* node) = 0;
    virtual void visitMulExp(MulExpNode* node) = 0;
    virtual void visitUnaryExp(UnaryExpNode* node) = 0;
    virtual void visitPrimaryExp(PrimaryExpNode* node) = 0;
    virtual void visitLVal(LValNode* node) = 0;
    virtual void visitNumber(NumberNode* node) = 0;
    virtual void visitFuncCall(FuncCallNode* node) = 0;
    virtual void visitRelExp(RelExpNode* node) = 0;
    virtual void visitEqExp(EqExpNode* node) = 0;
    virtual void visitLAndExp(LAndExpNode* node) = 0;
    virtual void visitLOrExp(LOrExpNode* node) = 0;
    virtual void visitCond(CondNode* node) = 0;
    virtual void visitConstExp(ConstExpNode* node) = 0;
};

} // namespace sysy

