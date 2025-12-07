#include "Nodes.h"
#include "ASTVisitor.h"

namespace sysy {

void CompUnitNode::accept(ASTVisitor* visitor) {
    visitor->visitCompUnit(this);
}

void ConstDeclNode::accept(ASTVisitor* visitor) {
    visitor->visitConstDecl(this);
}

void VarDeclNode::accept(ASTVisitor* visitor) {
    visitor->visitVarDecl(this);
}

void ConstDefNode::accept(ASTVisitor* visitor) {
    visitor->visitConstDef(this);
}

void VarDefNode::accept(ASTVisitor* visitor) {
    visitor->visitVarDef(this);
}

void ConstInitValNode::accept(ASTVisitor* visitor) {
    visitor->visitConstInitVal(this);
}

void InitValNode::accept(ASTVisitor* visitor) {
    visitor->visitInitVal(this);
}

void FuncDefNode::accept(ASTVisitor* visitor) {
    visitor->visitFuncDef(this);
}

void FuncFParamNode::accept(ASTVisitor* visitor) {
    visitor->visitFuncFParam(this);
}

void BlockStmtNode::accept(ASTVisitor* visitor) {
    visitor->visitBlockStmt(this);
}

void BlockItemNode::accept(ASTVisitor* visitor) {
    visitor->visitBlockItem(this);
}

void AssignStmtNode::accept(ASTVisitor* visitor) {
    visitor->visitAssignStmt(this);
}

void ExpStmtNode::accept(ASTVisitor* visitor) {
    visitor->visitExpStmt(this);
}

void IfStmtNode::accept(ASTVisitor* visitor) {
    visitor->visitIfStmt(this);
}

void WhileStmtNode::accept(ASTVisitor* visitor) {
    visitor->visitWhileStmt(this);
}

void BreakStmtNode::accept(ASTVisitor* visitor) {
    visitor->visitBreakStmt(this);
}

void ContinueStmtNode::accept(ASTVisitor* visitor) {
    visitor->visitContinueStmt(this);
}

void ReturnStmtNode::accept(ASTVisitor* visitor) {
    visitor->visitReturnStmt(this);
}

void AddExpNode::accept(ASTVisitor* visitor) {
    visitor->visitAddExp(this);
}

void MulExpNode::accept(ASTVisitor* visitor) {
    visitor->visitMulExp(this);
}

void UnaryExpNode::accept(ASTVisitor* visitor) {
    visitor->visitUnaryExp(this);
}

void PrimaryExpNode::accept(ASTVisitor* visitor) {
    visitor->visitPrimaryExp(this);
}

void LValNode::accept(ASTVisitor* visitor) {
    visitor->visitLVal(this);
}

void NumberNode::accept(ASTVisitor* visitor) {
    visitor->visitNumber(this);
}

void FuncCallNode::accept(ASTVisitor* visitor) {
    visitor->visitFuncCall(this);
}

void RelExpNode::accept(ASTVisitor* visitor) {
    visitor->visitRelExp(this);
}

void EqExpNode::accept(ASTVisitor* visitor) {
    visitor->visitEqExp(this);
}

void LAndExpNode::accept(ASTVisitor* visitor) {
    visitor->visitLAndExp(this);
}

void LOrExpNode::accept(ASTVisitor* visitor) {
    visitor->visitLOrExp(this);
}

void CondNode::accept(ASTVisitor* visitor) {
    visitor->visitCond(this);
}

void ConstExpNode::accept(ASTVisitor* visitor) {
    visitor->visitConstExp(this);
}

} // namespace sysy

