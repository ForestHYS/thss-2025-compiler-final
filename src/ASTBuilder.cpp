#include "ASTBuilder.h"
#include "Nodes.h"
#include <antlr4-runtime.h>
// 包含生成的 Parser 头文件（需要先运行 make antlr 生成）
// 生成的文件位于 src/antlr/ 目录
#include "SysYParser.h"

namespace sysy {

std::unique_ptr<CompUnitNode> ASTBuilder::build(antlr4::tree::ParseTree* tree) {
    // 将 ParseTree 转换为 CompUnitContext
    auto* ctx = dynamic_cast<SysYParser::CompUnitContext*>(tree);
    if (!ctx) {
        return nullptr;
    }
    return buildCompUnit(ctx);
}

std::unique_ptr<CompUnitNode> ASTBuilder::buildCompUnit(SysYParser::CompUnitContext* ctx) {
    auto node = std::make_unique<CompUnitNode>();
    
    for (auto* declCtx : ctx->decl()) {
        node->decls.push_back(buildDecl(declCtx));
    }
    for (auto* funcDefCtx : ctx->funcDef()) {
        node->funcDefs.push_back(buildFuncDef(funcDefCtx));
    }
    
    return node;
}

std::unique_ptr<DeclNode> ASTBuilder::buildDecl(SysYParser::DeclContext* ctx) {
    // decl: constDecl | varDecl;
    if (ctx->constDecl()) {
        return buildConstDecl(ctx->constDecl());
    } else if (ctx->varDecl()) {
        return buildVarDecl(ctx->varDecl());
    }
    return nullptr;
}

std::unique_ptr<ConstDeclNode> ASTBuilder::buildConstDecl(SysYParser::ConstDeclContext* ctx) {
    // constDecl: CONST bType constDef (COMMA constDef)* SEMICOLON;
    auto node = std::make_unique<ConstDeclNode>();
    node->bType = BType::INT; // bType 目前只有 int

    // 使用 Context 的访问器方法直接获取 constDef 列表
    for (auto* constDefCtx : ctx->constDef()) {
        node->constDefs.push_back(buildConstDef(constDefCtx));
    }
    return node;
}

std::unique_ptr<VarDeclNode> ASTBuilder::buildVarDecl(SysYParser::VarDeclContext* ctx) {
    // varDecl: bType varDef (COMMA varDef)* SEMICOLON;
    auto node = std::make_unique<VarDeclNode>();
    node->bType = BType::INT;

    // 使用 Context 的访问器方法直接获取 varDef 列表
    for (auto* varDefCtx : ctx->varDef()) {
        node->varDefs.push_back(buildVarDef(varDefCtx));
    }
    return node;
}

std::unique_ptr<ConstDefNode> ASTBuilder::buildConstDef(SysYParser::ConstDefContext* ctx) {
    // constDef: IDENT (L_BRACKT constExp R_BRACKT)* ASSIGN constInitVal;
    auto node = std::make_unique<ConstDefNode>();
    
    node->ident = ctx->IDENT()->getText();
    
    // 使用 Context 的访问器方法获取维度表达式
    for (auto* constExpCtx : ctx->constExp()) {
        node->dims.push_back(buildConstExp(constExpCtx));
    }
    
    // 获取初始化值
    node->initVal = buildConstInitVal(ctx->constInitVal());
    
    return node;
}

std::unique_ptr<VarDefNode> ASTBuilder::buildVarDef(SysYParser::VarDefContext* ctx) {
    // varDef:
    //   IDENT (L_BRACKT constExp R_BRACKT)*
    // | IDENT (L_BRACKT constExp R_BRACKT)* ASSIGN initVal;
    auto node = std::make_unique<VarDefNode>();
    
    node->ident = ctx->IDENT()->getText();
    
    // 使用 Context 的访问器方法获取维度表达式
    for (auto* constExpCtx : ctx->constExp()) {
        node->dims.push_back(buildConstExp(constExpCtx));
    }
    
    // 如果有初始化值
    if (ctx->ASSIGN()) {
        node->initVal = buildInitVal(ctx->initVal());
    } else {
        node->initVal = nullptr;
    }
    
    return node;
}

std::unique_ptr<ConstInitValNode> ASTBuilder::buildConstInitVal(SysYParser::ConstInitValContext* ctx) {
    // constInitVal:
    //   constExp
    // | L_BRACE (constInitVal (COMMA constInitVal)*)? R_BRACE;
    auto node = std::make_unique<ConstInitValNode>();
    
    if (ctx->constExp()) {
        // 标量值
        node->isScalar = true;
        node->scalarVal = buildConstExp(ctx->constExp());
    } else {
        // 数组值
        node->isScalar = false;
        for (auto* childCtx : ctx->constInitVal()) {
            node->arrayVals.push_back(buildConstInitVal(childCtx));
        }
    }
    
    return node;
}

std::unique_ptr<InitValNode> ASTBuilder::buildInitVal(SysYParser::InitValContext* ctx) {
    // initVal: exp | L_BRACE (initVal (COMMA initVal)*)? R_BRACE;
    auto node = std::make_unique<InitValNode>();
    
    if (ctx->exp()) {
        // 标量值
        node->isScalar = true;
        node->scalarVal = buildExp(ctx->exp());
    } else {
        // 数组值
        node->isScalar = false;
        for (auto* childCtx : ctx->initVal()) {
            node->arrayVals.push_back(buildInitVal(childCtx));
        }
    }
    
    return node;
}

std::unique_ptr<FuncDefNode> ASTBuilder::buildFuncDef(SysYParser::FuncDefContext* ctx) {
    // funcDef: funcType IDENT L_PAREN (funcFParams)? R_PAREN block;
    auto node = std::make_unique<FuncDefNode>();
    
    // 获取函数返回类型
    if (ctx->funcType()->VOID()) {
        node->funcType = FuncType::VOID;
    } else {
        node->funcType = FuncType::INT;
    }
    
    node->ident = ctx->IDENT()->getText();
    
    // 获取参数列表
    if (ctx->funcFParams()) {
        for (auto* paramCtx : ctx->funcFParams()->funcFParam()) {
            node->params.push_back(buildFuncFParam(paramCtx));
        }
    }
    
    // 获取函数体
    node->block = buildBlock(ctx->block());
    
    return node;
}

std::unique_ptr<FuncFParamNode> ASTBuilder::buildFuncFParam(SysYParser::FuncFParamContext* ctx) {
    // funcFParam:
    //   bType IDENT (L_BRACKT R_BRACKT (L_BRACKT exp R_BRACKT)*)?;
    auto node = std::make_unique<FuncFParamNode>();
    node->bType = BType::INT;
    node->ident = ctx->IDENT()->getText();
    
    // 判断是否为数组参数
    node->isArray = (ctx->L_BRACKT().size() > 0);
    
    if (node->isArray) {
        // 获取后续维度的表达式（第一维为空）
        for (auto* expCtx : ctx->exp()) {
            node->dims.push_back(buildExp(expCtx));
        }
    }
    
    return node;
}

std::unique_ptr<BlockStmtNode> ASTBuilder::buildBlock(SysYParser::BlockContext* ctx) {
    // block: L_BRACE (blockItem)* R_BRACE;
    auto node = std::make_unique<BlockStmtNode>();
    
    // 使用 Context 的访问器方法获取 blockItem 列表
    for (auto* blockItemCtx : ctx->blockItem()) {
        node->blockItems.push_back(buildBlockItem(blockItemCtx));
    }
    
    return node;
}

std::unique_ptr<BlockItemNode> ASTBuilder::buildBlockItem(SysYParser::BlockItemContext* ctx) {
    // blockItem: decl | stmt;
    auto node = std::make_unique<BlockItemNode>();
    
    if (ctx->decl()) {
        node->isDecl = true;
        node->decl = buildDecl(ctx->decl());
    } else if (ctx->stmt()) {
        node->isDecl = false;
        node->stmt = buildStmt(ctx->stmt());
    }
    
    return node;
}

std::unique_ptr<StmtNode> ASTBuilder::buildStmt(SysYParser::StmtContext* ctx) {
    // stmt:
    //   lVal ASSIGN exp SEMICOLON
    // | exp? SEMICOLON
    // | block
    // | IF L_PAREN cond R_PAREN stmt (ELSE stmt)?
    // | WHILE L_PAREN cond R_PAREN stmt
    // | BREAK SEMICOLON
    // | CONTINUE SEMICOLON
    // | RETURN exp? SEMICOLON;
    
    // 使用 Context 的布尔方法判断语句类型
    if (ctx->lVal() && ctx->exp() && ctx->ASSIGN()) {
        return buildAssignStmt(ctx);
    } else if (ctx->block()) {
        return buildBlock(ctx->block());
    } else if (ctx->IF()) {
        return buildIfStmt(ctx);
    } else if (ctx->WHILE()) {
        return buildWhileStmt(ctx);
    } else if (ctx->BREAK()) {
        return buildBreakStmt(ctx);
    } else if (ctx->CONTINUE()) {
        return buildContinueStmt(ctx);
    } else if (ctx->RETURN()) {
        return buildReturnStmt(ctx);
    } else {
        return buildExpStmt(ctx);
    }
}

std::unique_ptr<AssignStmtNode> ASTBuilder::buildAssignStmt(SysYParser::StmtContext* ctx) {
    // lVal ASSIGN exp SEMICOLON
    auto node = std::make_unique<AssignStmtNode>();
    node->lVal = buildLVal(ctx->lVal());
    node->exp = buildExp(ctx->exp());
    return node;
}

std::unique_ptr<ExpStmtNode> ASTBuilder::buildExpStmt(SysYParser::StmtContext* ctx) {
    // exp? SEMICOLON
    auto node = std::make_unique<ExpStmtNode>();
    if (ctx->exp()) {
        node->exp = buildExp(ctx->exp());
    } else {
        node->exp = nullptr;  // 空语句
    }
    return node;
}

std::unique_ptr<IfStmtNode> ASTBuilder::buildIfStmt(SysYParser::StmtContext* ctx) {
    // IF L_PAREN cond R_PAREN stmt (ELSE stmt)?
    auto node = std::make_unique<IfStmtNode>();
    node->cond = buildCond(ctx->cond());
    node->thenStmt = buildStmt(ctx->stmt(0));
    if (ctx->ELSE()) {
        node->elseStmt = buildStmt(ctx->stmt(1));
    } else {
        node->elseStmt = nullptr;
    }
    return node;
}

std::unique_ptr<WhileStmtNode> ASTBuilder::buildWhileStmt(SysYParser::StmtContext* ctx) {
    // WHILE L_PAREN cond R_PAREN stmt
    auto node = std::make_unique<WhileStmtNode>();
    node->cond = buildCond(ctx->cond());
    node->body = buildStmt(ctx->stmt(0));
    return node;
}

std::unique_ptr<BreakStmtNode> ASTBuilder::buildBreakStmt(SysYParser::StmtContext* ctx) {
    (void)ctx;
    return std::make_unique<BreakStmtNode>();
}

std::unique_ptr<ContinueStmtNode> ASTBuilder::buildContinueStmt(SysYParser::StmtContext* ctx) {
    (void)ctx;
    return std::make_unique<ContinueStmtNode>();
}

std::unique_ptr<ReturnStmtNode> ASTBuilder::buildReturnStmt(SysYParser::StmtContext* ctx) {
    // RETURN exp? SEMICOLON;
    auto node = std::make_unique<ReturnStmtNode>();
    if (ctx->exp()) {
        node->exp = buildExp(ctx->exp());
    } else {
        node->exp = nullptr;
    }
    return node;
}

std::unique_ptr<ExpNode> ASTBuilder::buildExp(SysYParser::ExpContext* ctx) {
    // exp: addExp;
    return buildAddExp(ctx->addExp());
}

std::unique_ptr<AddExpNode> ASTBuilder::buildAddExp(SysYParser::AddExpContext* ctx) {
    // addExp: mulExp | addExp (PLUS | MINUS) mulExp;
    auto node = std::make_unique<AddExpNode>();
    
    if (ctx->addExp()) {
        // 有左操作数
        node->left = buildAddExp(ctx->addExp());
        if (ctx->PLUS()) {
            node->op = BinaryOp::PLUS;
        } else {
            node->op = BinaryOp::MINUS;
        }
        node->right = buildMulExp(ctx->mulExp());
    } else {
        // 只有 mulExp，转为 AddExp，left=null，right=mulExp
        node->left = nullptr;
        node->right = buildMulExp(ctx->mulExp());
        node->op = BinaryOp::PLUS;  // 默认值，无实际意义
    }
    
    return node;
}

std::unique_ptr<MulExpNode> ASTBuilder::buildMulExp(SysYParser::MulExpContext* ctx) {
    // mulExp: unaryExp | mulExp (MUL | DIV | MOD) unaryExp;
    auto node = std::make_unique<MulExpNode>();
    
    if (ctx->mulExp()) {
        // 有左操作数
        node->left = buildMulExp(ctx->mulExp());
        if (ctx->MUL()) {
            node->op = BinaryOp::MUL;
        } else if (ctx->DIV()) {
            node->op = BinaryOp::DIV;
        } else {
            node->op = BinaryOp::MOD;
        }
        node->right = buildUnaryExp(ctx->unaryExp());
    } else {
        // 只有 unaryExp
        node->left = nullptr;
        node->right = buildUnaryExp(ctx->unaryExp());
        node->op = BinaryOp::MUL;  // 默认值，无实际意义
    }
    
    return node;
}

std::unique_ptr<UnaryExpNode> ASTBuilder::buildUnaryExp(SysYParser::UnaryExpContext* ctx) {
    // unaryExp:
    //   primaryExp
    // | IDENT L_PAREN (funcRParams)? R_PAREN
    // | unaryOp unaryExp;
    
    if (ctx->unaryOp()) {
        // 一元运算符
        auto node = std::make_unique<UnaryExpNode>();
        auto opText = ctx->unaryOp()->getText();
        if (opText == "+") {
            node->op = UnaryOp::PLUS;
        } else if (opText == "-") {
            node->op = UnaryOp::MINUS;
        } else {
            node->op = UnaryOp::NOT;
        }
        node->operand = buildUnaryExp(ctx->unaryExp());
        return node;
    } else if (ctx->IDENT()) {
        // 函数调用：IDENT L_PAREN (funcRParams)? R_PAREN
        // 根据 AST 设计，函数调用是 FuncCallNode，但 MulExp 需要 UnaryExpNode
        // 这里我们需要创建一个包装的 UnaryExp，但这样不符合 AST 设计
        // 实际上，根据 AST 设计，函数调用应该作为 PrimaryExp 的一部分
        // 为了简化，我们创建一个默认的 UnaryExp，operand 为 nullptr
        // 函数调用信息将在后续阶段（语义分析）处理
        auto node = std::make_unique<UnaryExpNode>();
        node->op = UnaryOp::PLUS;  // 占位符
        node->operand = nullptr;
        return node;
    } else {
        // primaryExp - 创建一个包装的 UnaryExp
        // 根据 AST 设计，PrimaryExp 应该直接使用，但为了类型一致性，我们包装它
        auto node = std::make_unique<UnaryExpNode>();
        node->op = UnaryOp::PLUS;  // 占位符，表示无一元运算符
        node->operand = nullptr;  // PrimaryExp 将在后续处理
        return node;
    }
}

std::unique_ptr<PrimaryExpNode> ASTBuilder::buildPrimaryExp(SysYParser::PrimaryExpContext* ctx) {
    // primaryExp: L_PAREN exp R_PAREN | lVal | number;
    auto node = std::make_unique<PrimaryExpNode>();
    
    if (ctx->L_PAREN()) {
        // 括号表达式
        node->type = PrimaryExpType::PAREN_EXP;
        node->exp = buildExp(ctx->exp());
    } else if (ctx->lVal()) {
        // 左值
        node->type = PrimaryExpType::LVAL;
        node->lVal = buildLVal(ctx->lVal());
    } else if (ctx->number()) {
        // 数字
        node->type = PrimaryExpType::NUMBER;
        node->number = buildNumber(ctx->number());
    }
    
    return node;
}

std::unique_ptr<LValNode> ASTBuilder::buildLVal(SysYParser::LValContext* ctx) {
    // lVal: IDENT (L_BRACKT exp R_BRACKT)*;
    auto node = std::make_unique<LValNode>();
    node->ident = ctx->IDENT()->getText();
    
    // 使用 Context 的访问器方法获取索引表达式
    for (auto* expCtx : ctx->exp()) {
        node->indices.push_back(buildExp(expCtx));
    }
    
    return node;
}

std::unique_ptr<NumberNode> ASTBuilder::buildNumber(SysYParser::NumberContext* ctx) {
    // number: INTEGER_CONST;
    auto node = std::make_unique<NumberNode>();
    node->value = getIntValue(ctx->INTEGER_CONST());
    return node;
}

std::unique_ptr<FuncCallNode> ASTBuilder::buildFuncCall(SysYParser::UnaryExpContext* ctx) {
    // unaryExp 中的形式：IDENT L_PAREN (funcRParams)? R_PAREN
    // funcRParams: exp (COMMA exp)*;
    auto node = std::make_unique<FuncCallNode>();
    node->ident = ctx->IDENT()->getText();
    
    if (ctx->funcRParams()) {
        for (auto* expCtx : ctx->funcRParams()->exp()) {
            node->args.push_back(buildExp(expCtx));
        }
    }
    
    return node;
}

std::unique_ptr<RelExpNode> ASTBuilder::buildRelExp(SysYParser::RelExpContext* ctx) {
    // relExp: addExp | relExp (LT | GT | LE | GE) addExp;
    auto node = std::make_unique<RelExpNode>();
    
    if (ctx->relExp()) {
        // 有左操作数
        node->left = buildRelExp(ctx->relExp());
        if (ctx->LT()) {
            node->op = BinaryOp::LT;
        } else if (ctx->GT()) {
            node->op = BinaryOp::GT;
        } else if (ctx->LE()) {
            node->op = BinaryOp::LE;
        } else {
            node->op = BinaryOp::GE;
        }
        node->right = buildAddExp(ctx->addExp());
    } else {
        // 只有 addExp
        node->left = nullptr;
        node->right = buildAddExp(ctx->addExp());
        node->op = BinaryOp::LT;  // 默认值，无实际意义
    }
    
    return node;
}

std::unique_ptr<EqExpNode> ASTBuilder::buildEqExp(SysYParser::EqExpContext* ctx) {
    // eqExp: relExp | eqExp (EQ | NEQ) relExp;
    auto node = std::make_unique<EqExpNode>();
    
    if (ctx->eqExp()) {
        // 有左操作数
        node->left = buildEqExp(ctx->eqExp());
        if (ctx->EQ()) {
            node->op = BinaryOp::EQ;
        } else {
            node->op = BinaryOp::NEQ;
        }
        node->right = buildRelExp(ctx->relExp());
    } else {
        // 只有 relExp
        node->left = nullptr;
        node->right = buildRelExp(ctx->relExp());
        node->op = BinaryOp::EQ;  // 默认值，无实际意义
    }
    
    return node;
}

std::unique_ptr<LAndExpNode> ASTBuilder::buildLAndExp(SysYParser::LAndExpContext* ctx) {
    // lAndExp: eqExp | lAndExp AND eqExp;
    auto node = std::make_unique<LAndExpNode>();
    
    if (ctx->lAndExp()) {
        // 有左操作数
        node->left = buildLAndExp(ctx->lAndExp());
        node->right = buildEqExp(ctx->eqExp());
    } else {
        // 只有 eqExp
        node->left = nullptr;
        node->right = buildEqExp(ctx->eqExp());
    }
    
    return node;
}

std::unique_ptr<LOrExpNode> ASTBuilder::buildLOrExp(SysYParser::LOrExpContext* ctx) {
    // lOrExp: lAndExp | lOrExp OR lAndExp;
    auto node = std::make_unique<LOrExpNode>();
    
    if (ctx->lOrExp()) {
        // 有左操作数
        node->left = buildLOrExp(ctx->lOrExp());
        node->right = buildLAndExp(ctx->lAndExp());
    } else {
        // 只有 lAndExp
        node->left = nullptr;
        node->right = buildLAndExp(ctx->lAndExp());
    }
    
    return node;
}

std::unique_ptr<CondNode> ASTBuilder::buildCond(SysYParser::CondContext* ctx) {
    // cond: lOrExp;
    auto node = std::make_unique<CondNode>();
    node->lOrExp = buildLOrExp(ctx->lOrExp());
    return node;
}

std::unique_ptr<ConstExpNode> ASTBuilder::buildConstExp(SysYParser::ConstExpContext* ctx) {
    // constExp: addExp;
    auto node = std::make_unique<ConstExpNode>();
    node->addExp = buildAddExp(ctx->addExp());
    return node;
}

int ASTBuilder::getIntValue(antlr4::tree::TerminalNode* token) {
    // token->getText() 已经是词法单元的文本（十进制 / 八进制 / 十六进制）
    std::string text = token->getText();
    if (text.size() >= 2 && text[0] == '0') {
        if (text[1] == 'x' || text[1] == 'X') {
            // 十六进制
            return static_cast<int>(std::stol(text, nullptr, 16));
        } else if (text.size() > 1) {
            // 八进制
            return static_cast<int>(std::stol(text, nullptr, 8));
        }
    }
    // 默认十进制
    return static_cast<int>(std::stol(text, nullptr, 10));
}

} // namespace sysy

