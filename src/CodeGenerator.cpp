#include "CodeGenerator.h"
#include "Nodes.h"
#include <sstream>

namespace sysy {

CodeGenerator::CodeGenerator(SymbolTableManager* symTabMgr)
    : symbolTableManager(symTabMgr), labelCounter(0) {
}

CodeGenerator::~CodeGenerator() = default;

std::string CodeGenerator::generate(CompUnitNode* root) {
    irStream.str("");  // 清空流
    irStream.clear();
    
    // 生成模块头部
    irStream << "; ModuleID = 'sysy_module'\n";
    irStream << "source_filename = \"sysy\"\n\n";
    
    root->accept(this);
    
    return irStream.str();
}

std::string CodeGenerator::getLLVMType(DataType dataType) {
    switch (dataType) {
        case DataType::INT:
            return "i32";
        case DataType::VOID:
            return "void";
        case DataType::INT_ARRAY:
            return "i32*";  // 数组作为指针
        default:
            return "i32";
    }
}

std::string CodeGenerator::getLLVMArrayType(DataType elementType, const std::vector<int>& dimensions) {
    std::string baseType = getLLVMType(elementType);
    // 从内到外构建数组类型，例如 [2 x [3 x i32]]
    for (auto it = dimensions.rbegin(); it != dimensions.rend(); ++it) {
        baseType = "[" + std::to_string(*it) + " x " + baseType + "]";
    }
    return baseType;
}

std::string CodeGenerator::generateLabel(const std::string& prefix) {
    return prefix + std::to_string(labelCounter++);
}

std::string CodeGenerator::generateVarName(const std::string& name) {
    // 处理变量名，确保符合LLVM IR命名规范
    return "%" + name;
}

// 实现所有visit方法（框架代码，不做实际工作）
void CodeGenerator::visitCompUnit(CompUnitNode* node) {
    // TODO: 实现编译单元代码生成
}

void CodeGenerator::visitConstDecl(ConstDeclNode* node) {
    // TODO: 实现常量声明代码生成
}

void CodeGenerator::visitVarDecl(VarDeclNode* node) {
    // TODO: 实现变量声明代码生成
}

void CodeGenerator::visitConstDef(ConstDefNode* node) {
    // TODO: 实现常量定义代码生成
}

void CodeGenerator::visitVarDef(VarDefNode* node) {
    // TODO: 实现变量定义代码生成
}

void CodeGenerator::visitConstInitVal(ConstInitValNode* node) {
    // TODO: 实现常量初始化值代码生成
}

void CodeGenerator::visitInitVal(InitValNode* node) {
    // TODO: 实现变量初始化值代码生成
}

void CodeGenerator::visitFuncDef(FuncDefNode* node) {
    // TODO: 实现函数定义代码生成
}

void CodeGenerator::visitFuncFParam(FuncFParamNode* node) {
    // TODO: 实现函数参数代码生成
}

void CodeGenerator::visitBlockStmt(BlockStmtNode* node) {
    // TODO: 实现块语句代码生成
}

void CodeGenerator::visitBlockItem(BlockItemNode* node) {
    // TODO: 实现块项目代码生成
}

void CodeGenerator::visitAssignStmt(AssignStmtNode* node) {
    // TODO: 实现赋值语句代码生成
}

void CodeGenerator::visitExpStmt(ExpStmtNode* node) {
    // TODO: 实现表达式语句代码生成
}

void CodeGenerator::visitIfStmt(IfStmtNode* node) {
    // TODO: 实现if语句代码生成
}

void CodeGenerator::visitWhileStmt(WhileStmtNode* node) {
    // TODO: 实现while语句代码生成
}

void CodeGenerator::visitBreakStmt(BreakStmtNode* node) {
    // TODO: 实现break语句代码生成
}

void CodeGenerator::visitContinueStmt(ContinueStmtNode* node) {
    // TODO: 实现continue语句代码生成
}

void CodeGenerator::visitReturnStmt(ReturnStmtNode* node) {
    // TODO: 实现return语句代码生成
}

void CodeGenerator::visitAddExp(AddExpNode* node) {
    // TODO: 实现加减表达式代码生成
}

void CodeGenerator::visitMulExp(MulExpNode* node) {
    // TODO: 实现乘除模表达式代码生成
}

void CodeGenerator::visitUnaryExp(UnaryExpNode* node) {
    // TODO: 实现一元表达式代码生成
}

void CodeGenerator::visitPrimaryExp(PrimaryExpNode* node) {
    // TODO: 实现基本表达式代码生成
}

void CodeGenerator::visitLVal(LValNode* node) {
    // TODO: 实现左值代码生成
}

void CodeGenerator::visitNumber(NumberNode* node) {
    // TODO: 实现数字字面量代码生成
}

void CodeGenerator::visitFuncCall(FuncCallNode* node) {
    // TODO: 实现函数调用代码生成
}

void CodeGenerator::visitRelExp(RelExpNode* node) {
    // TODO: 实现关系表达式代码生成
}

void CodeGenerator::visitEqExp(EqExpNode* node) {
    // TODO: 实现相等表达式代码生成
}

void CodeGenerator::visitLAndExp(LAndExpNode* node) {
    // TODO: 实现逻辑与表达式代码生成
}

void CodeGenerator::visitLOrExp(LOrExpNode* node) {
    // TODO: 实现逻辑或表达式代码生成
}

void CodeGenerator::visitCond(CondNode* node) {
    // TODO: 实现条件表达式代码生成
}

void CodeGenerator::visitConstExp(ConstExpNode* node) {
    // TODO: 实现常量表达式代码生成
}

} // namespace sysy

