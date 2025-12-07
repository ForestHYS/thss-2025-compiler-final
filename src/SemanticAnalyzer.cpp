#include "SemanticAnalyzer.h"
#include "Nodes.h"

namespace sysy {

SemanticAnalyzer::SemanticAnalyzer(SymbolTableManager* symTabMgr)
    : symbolTableManager(symTabMgr) {
}

SemanticAnalyzer::~SemanticAnalyzer() = default;

void SemanticAnalyzer::analyze(CompUnitNode* root) {
    root->accept(this);
}

void SemanticAnalyzer::checkType(ExpNode* exp, DataType expectedType) {
    // TODO: 实现类型检查
}

int SemanticAnalyzer::evaluateConstExp(ConstExpNode* node) {
    // TODO: 实现常量表达式求值
    return 0;
}

// 实现所有visit方法（框架代码）
void SemanticAnalyzer::visitCompUnit(CompUnitNode* node) {
    // TODO: 实现编译单元语义分析
}

void SemanticAnalyzer::visitConstDecl(ConstDeclNode* node) {
    // TODO: 实现常量声明语义分析
}

void SemanticAnalyzer::visitVarDecl(VarDeclNode* node) {
    // TODO: 实现变量声明语义分析
}

void SemanticAnalyzer::visitConstDef(ConstDefNode* node) {
    // TODO: 实现常量定义语义分析
}

void SemanticAnalyzer::visitVarDef(VarDefNode* node) {
    // TODO: 实现变量定义语义分析
}

void SemanticAnalyzer::visitConstInitVal(ConstInitValNode* node) {
    // TODO: 实现常量初始化值语义分析
}

void SemanticAnalyzer::visitInitVal(InitValNode* node) {
    // TODO: 实现变量初始化值语义分析
}

void SemanticAnalyzer::visitFuncDef(FuncDefNode* node) {
    // TODO: 实现函数定义语义分析
}

void SemanticAnalyzer::visitFuncFParam(FuncFParamNode* node) {
    // TODO: 实现函数参数语义分析
}

void SemanticAnalyzer::visitBlockStmt(BlockStmtNode* node) {
    // TODO: 实现块语句语义分析
}

void SemanticAnalyzer::visitBlockItem(BlockItemNode* node) {
    // TODO: 实现块项目语义分析
}

void SemanticAnalyzer::visitAssignStmt(AssignStmtNode* node) {
    // TODO: 实现赋值语句语义分析
}

void SemanticAnalyzer::visitExpStmt(ExpStmtNode* node) {
    // TODO: 实现表达式语句语义分析
}

void SemanticAnalyzer::visitIfStmt(IfStmtNode* node) {
    // TODO: 实现if语句语义分析
}

void SemanticAnalyzer::visitWhileStmt(WhileStmtNode* node) {
    // TODO: 实现while语句语义分析
}

void SemanticAnalyzer::visitBreakStmt(BreakStmtNode* node) {
    // TODO: 实现break语句语义分析
}

void SemanticAnalyzer::visitContinueStmt(ContinueStmtNode* node) {
    // TODO: 实现continue语句语义分析
}

void SemanticAnalyzer::visitReturnStmt(ReturnStmtNode* node) {
    // TODO: 实现return语句语义分析
}

void SemanticAnalyzer::visitAddExp(AddExpNode* node) {
    // TODO: 实现加减表达式语义分析
}

void SemanticAnalyzer::visitMulExp(MulExpNode* node) {
    // TODO: 实现乘除模表达式语义分析
}

void SemanticAnalyzer::visitUnaryExp(UnaryExpNode* node) {
    // TODO: 实现一元表达式语义分析
}

void SemanticAnalyzer::visitPrimaryExp(PrimaryExpNode* node) {
    // TODO: 实现基本表达式语义分析
}

void SemanticAnalyzer::visitLVal(LValNode* node) {
    // TODO: 实现左值语义分析
}

void SemanticAnalyzer::visitNumber(NumberNode* node) {
    // TODO: 实现数字字面量语义分析
}

void SemanticAnalyzer::visitFuncCall(FuncCallNode* node) {
    // TODO: 实现函数调用语义分析
}

void SemanticAnalyzer::visitRelExp(RelExpNode* node) {
    // TODO: 实现关系表达式语义分析
}

void SemanticAnalyzer::visitEqExp(EqExpNode* node) {
    // TODO: 实现相等表达式语义分析
}

void SemanticAnalyzer::visitLAndExp(LAndExpNode* node) {
    // TODO: 实现逻辑与表达式语义分析
}

void SemanticAnalyzer::visitLOrExp(LOrExpNode* node) {
    // TODO: 实现逻辑或表达式语义分析
}

void SemanticAnalyzer::visitCond(CondNode* node) {
    // TODO: 实现条件表达式语义分析
}

void SemanticAnalyzer::visitConstExp(ConstExpNode* node) {
    // TODO: 实现常量表达式语义分析
}

} // namespace sysy

