#pragma once

#include "ASTNode.h"
#include "ASTTypes.h"
#include <string>
#include <vector>
#include <memory>

namespace sysy {

// 前向声明
class ASTVisitor;
class DeclNode;
class FuncDefNode;
class ConstExpNode;
class ExpNode;
class CondNode;
class StmtNode;
class BlockItemNode;
class LValNode;
class ConstDefNode;
class VarDefNode;
class ConstInitValNode;
class InitValNode;
class FuncFParamNode;
class BlockStmtNode;
class MulExpNode;
class UnaryExpNode;
class NumberNode;

/**
 * 编译单元节点
 */
class CompUnitNode : public ASTNode {
public:
    std::vector<std::unique_ptr<DeclNode>> decls;
    std::vector<std::unique_ptr<FuncDefNode>> funcDefs;
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "CompUnit"; }
};

/**
 * 声明节点基类
 */
class DeclNode : public ASTNode {
public:
    virtual ~DeclNode() = default;
};

/**
 * 常量声明节点
 */
class ConstDeclNode : public DeclNode {
public:
    BType bType;
    std::vector<std::unique_ptr<ConstDefNode>> constDefs;
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "ConstDecl"; }
};

/**
 * 变量声明节点
 */
class VarDeclNode : public DeclNode {
public:
    BType bType;
    std::vector<std::unique_ptr<VarDefNode>> varDefs;
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "VarDecl"; }
};

/**
 * 常量定义节点
 */
class ConstDefNode : public ASTNode {
public:
    std::string ident;
    std::vector<std::unique_ptr<ConstExpNode>> dims;
    std::unique_ptr<ConstInitValNode> initVal;
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "ConstDef"; }
};

/**
 * 变量定义节点
 */
class VarDefNode : public ASTNode {
public:
    std::string ident;
    std::vector<std::unique_ptr<ConstExpNode>> dims;
    std::unique_ptr<InitValNode> initVal;  // nullptr表示未初始化
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "VarDef"; }
};

/**
 * 常量初始化值节点
 */
class ConstInitValNode : public ASTNode {
public:
    bool isScalar;
    std::unique_ptr<ConstExpNode> scalarVal;  // 当isScalar为true时使用
    std::vector<std::unique_ptr<ConstInitValNode>> arrayVals;  // 当isScalar为false时使用
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "ConstInitVal"; }
};

/**
 * 变量初始化值节点
 */
class InitValNode : public ASTNode {
public:
    bool isScalar;
    std::unique_ptr<ExpNode> scalarVal;  // 当isScalar为true时使用
    std::vector<std::unique_ptr<InitValNode>> arrayVals;  // 当isScalar为false时使用
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "InitVal"; }
};

/**
 * 函数定义节点
 */
class FuncDefNode : public ASTNode {
public:
    FuncType funcType;
    std::string ident;
    std::vector<std::unique_ptr<FuncFParamNode>> params;
    std::unique_ptr<BlockStmtNode> block;
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "FuncDef"; }
};

/**
 * 函数形式参数节点
 */
class FuncFParamNode : public ASTNode {
public:
    BType bType;
    std::string ident;
    bool isArray;
    std::vector<std::unique_ptr<ExpNode>> dims;  // 第一维为空，后续维度有表达式
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "FuncFParam"; }
};

/**
 * 语句节点基类
 */
class StmtNode : public ASTNode {
public:
    virtual ~StmtNode() = default;
};

/**
 * 块语句节点
 */
class BlockStmtNode : public StmtNode {
public:
    std::vector<std::unique_ptr<BlockItemNode>> blockItems;
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "BlockStmt"; }
};

/**
 * 块项目节点
 */
class BlockItemNode : public ASTNode {
public:
    bool isDecl;
    std::unique_ptr<DeclNode> decl;  // 当isDecl为true时使用
    std::unique_ptr<StmtNode> stmt;  // 当isDecl为false时使用
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "BlockItem"; }
};

/**
 * 赋值语句节点
 */
class AssignStmtNode : public StmtNode {
public:
    std::unique_ptr<LValNode> lVal;
    std::unique_ptr<ExpNode> exp;
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "AssignStmt"; }
};

/**
 * 表达式语句节点
 */
class ExpStmtNode : public StmtNode {
public:
    std::unique_ptr<ExpNode> exp;  // nullptr表示空语句
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "ExpStmt"; }
};

/**
 * if语句节点
 */
class IfStmtNode : public StmtNode {
public:
    std::unique_ptr<CondNode> cond;
    std::unique_ptr<StmtNode> thenStmt;
    std::unique_ptr<StmtNode> elseStmt;  // nullptr表示无else分支
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "IfStmt"; }
};

/**
 * while语句节点
 */
class WhileStmtNode : public StmtNode {
public:
    std::unique_ptr<CondNode> cond;
    std::unique_ptr<StmtNode> body;
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "WhileStmt"; }
};

/**
 * break语句节点
 */
class BreakStmtNode : public StmtNode {
public:
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "BreakStmt"; }
};

/**
 * continue语句节点
 */
class ContinueStmtNode : public StmtNode {
public:
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "ContinueStmt"; }
};

/**
 * return语句节点
 */
class ReturnStmtNode : public StmtNode {
public:
    std::unique_ptr<ExpNode> exp;  // nullptr表示无返回值
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "ReturnStmt"; }
};

/**
 * 表达式节点基类
 */
class ExpNode : public ASTNode {
public:
    virtual ~ExpNode() = default;
};

/**
 * 加减表达式节点
 */
class AddExpNode : public ExpNode {
public:
    BinaryOp op;  // PLUS或MINUS
    std::unique_ptr<AddExpNode> left;  // nullptr表示单操作数
    std::unique_ptr<MulExpNode> right;
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "AddExp"; }
};

/**
 * 乘除模表达式节点
 */
class MulExpNode : public ExpNode {
public:
    BinaryOp op;  // MUL、DIV或MOD
    std::unique_ptr<MulExpNode> left;  // nullptr表示单操作数
    std::unique_ptr<UnaryExpNode> right;
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "MulExp"; }
};

/**
 * 一元表达式节点
 */
class UnaryExpNode : public ExpNode {
public:
    UnaryOp op;
    std::unique_ptr<UnaryExpNode> operand;  // 递归结构，最终指向PrimaryExpNode
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "UnaryExp"; }
};

/**
 * 基本表达式节点
 */
class PrimaryExpNode : public ExpNode {
public:
    PrimaryExpType type;
    std::unique_ptr<ExpNode> exp;  // 当type为PAREN_EXP时使用
    std::unique_ptr<LValNode> lVal;  // 当type为LVAL时使用
    std::unique_ptr<NumberNode> number;  // 当type为NUMBER时使用
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "PrimaryExp"; }
};

/**
 * 左值节点
 */
class LValNode : public ExpNode {
public:
    std::string ident;
    std::vector<std::unique_ptr<ExpNode>> indices;  // 数组索引表达式列表
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "LVal"; }
};

/**
 * 数字字面量节点
 */
class NumberNode : public ExpNode {
public:
    int value;
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "Number"; }
};

/**
 * 函数调用节点
 */
class FuncCallNode : public ExpNode {
public:
    std::string ident;
    std::vector<std::unique_ptr<ExpNode>> args;
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "FuncCall"; }
};

/**
 * 关系表达式节点
 */
class RelExpNode : public ExpNode {
public:
    BinaryOp op;  // LT、GT、LE或GE
    std::unique_ptr<RelExpNode> left;  // nullptr表示单操作数
    std::unique_ptr<AddExpNode> right;
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "RelExp"; }
};

/**
 * 相等表达式节点
 */
class EqExpNode : public ExpNode {
public:
    BinaryOp op;  // EQ或NEQ
    std::unique_ptr<EqExpNode> left;  // nullptr表示单操作数
    std::unique_ptr<RelExpNode> right;
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "EqExp"; }
};

/**
 * 逻辑与表达式节点
 */
class LAndExpNode : public ExpNode {
public:
    std::unique_ptr<LAndExpNode> left;  // nullptr表示单操作数
    std::unique_ptr<EqExpNode> right;
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "LAndExp"; }
};

/**
 * 逻辑或表达式节点
 */
class LOrExpNode : public ExpNode {
public:
    std::unique_ptr<LOrExpNode> left;  // nullptr表示单操作数
    std::unique_ptr<LAndExpNode> right;
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "LOrExp"; }
};

/**
 * 条件表达式节点
 */
class CondNode : public ASTNode {
public:
    std::unique_ptr<LOrExpNode> lOrExp;
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "Cond"; }
};

/**
 * 常量表达式节点
 */
class ConstExpNode : public ASTNode {
public:
    std::unique_ptr<AddExpNode> addExp;
    
    void accept(ASTVisitor* visitor) override;
    std::string getNodeType() const override { return "ConstExp"; }
};

} // namespace sysy

