#pragma once

#include "Nodes.h"
#include <memory>
#include <string>
#include <antlr4-runtime.h>
// 直接包含完整的 Parser 定义
#include "SysYParser.h"

namespace sysy {

/**
 * AST构建器
 * 从ANTLR ParseTree构建AST
 * 使用强类型的Context类替代通用的ParseTree，提供更好的类型安全性
 */
class ASTBuilder {
private:
    // 编译单元和声明
    std::unique_ptr<CompUnitNode> buildCompUnit(SysYParser::CompUnitContext* ctx);
    std::unique_ptr<DeclNode> buildDecl(SysYParser::DeclContext* ctx);
    std::unique_ptr<ConstDeclNode> buildConstDecl(SysYParser::ConstDeclContext* ctx);
    std::unique_ptr<VarDeclNode> buildVarDecl(SysYParser::VarDeclContext* ctx);
    std::unique_ptr<ConstDefNode> buildConstDef(SysYParser::ConstDefContext* ctx);
    std::unique_ptr<VarDefNode> buildVarDef(SysYParser::VarDefContext* ctx);
    std::unique_ptr<ConstInitValNode> buildConstInitVal(SysYParser::ConstInitValContext* ctx);
    std::unique_ptr<InitValNode> buildInitVal(SysYParser::InitValContext* ctx);
    
    // 函数
    std::unique_ptr<FuncDefNode> buildFuncDef(SysYParser::FuncDefContext* ctx);
    std::unique_ptr<FuncFParamNode> buildFuncFParam(SysYParser::FuncFParamContext* ctx);
    
    // 语句
    std::unique_ptr<BlockStmtNode> buildBlock(SysYParser::BlockContext* ctx);
    std::unique_ptr<BlockItemNode> buildBlockItem(SysYParser::BlockItemContext* ctx);
    std::unique_ptr<StmtNode> buildStmt(SysYParser::StmtContext* ctx);
    std::unique_ptr<AssignStmtNode> buildAssignStmt(SysYParser::StmtContext* ctx);
    std::unique_ptr<ExpStmtNode> buildExpStmt(SysYParser::StmtContext* ctx);
    std::unique_ptr<IfStmtNode> buildIfStmt(SysYParser::StmtContext* ctx);
    std::unique_ptr<WhileStmtNode> buildWhileStmt(SysYParser::StmtContext* ctx);
    std::unique_ptr<BreakStmtNode> buildBreakStmt(SysYParser::StmtContext* ctx);
    std::unique_ptr<ContinueStmtNode> buildContinueStmt(SysYParser::StmtContext* ctx);
    std::unique_ptr<ReturnStmtNode> buildReturnStmt(SysYParser::StmtContext* ctx);
    
    // 表达式
    std::unique_ptr<ExpNode> buildExp(SysYParser::ExpContext* ctx);
    std::unique_ptr<AddExpNode> buildAddExp(SysYParser::AddExpContext* ctx);
    std::unique_ptr<MulExpNode> buildMulExp(SysYParser::MulExpContext* ctx);
    std::unique_ptr<UnaryExpNode> buildUnaryExp(SysYParser::UnaryExpContext* ctx);
    std::unique_ptr<PrimaryExpNode> buildPrimaryExp(SysYParser::PrimaryExpContext* ctx);
    std::unique_ptr<LValNode> buildLVal(SysYParser::LValContext* ctx);
    std::unique_ptr<NumberNode> buildNumber(SysYParser::NumberContext* ctx);
    std::unique_ptr<FuncCallNode> buildFuncCall(SysYParser::UnaryExpContext* ctx);
    std::unique_ptr<RelExpNode> buildRelExp(SysYParser::RelExpContext* ctx);
    std::unique_ptr<EqExpNode> buildEqExp(SysYParser::EqExpContext* ctx);
    std::unique_ptr<LAndExpNode> buildLAndExp(SysYParser::LAndExpContext* ctx);
    std::unique_ptr<LOrExpNode> buildLOrExp(SysYParser::LOrExpContext* ctx);
    std::unique_ptr<CondNode> buildCond(SysYParser::CondContext* ctx);
    std::unique_ptr<ConstExpNode> buildConstExp(SysYParser::ConstExpContext* ctx);
    
    // 辅助方法：从Token获取整数值
    int getIntValue(antlr4::tree::TerminalNode* token);
    
public:
    ASTBuilder() = default;
    ~ASTBuilder() = default;
    
    /**
     * 从ParseTree构建AST
     * @param tree ANTLR ParseTree根节点（CompUnitContext*）
     * @return AST根节点
     */
    std::unique_ptr<CompUnitNode> build(antlr4::tree::ParseTree* tree);
};

} // namespace sysy

