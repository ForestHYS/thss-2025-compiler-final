#include "SemanticAnalyzer.h"
#include "Nodes.h"
#include <iostream>

namespace sysy {

SemanticAnalyzer::SemanticAnalyzer(SymbolTableManager* symTabMgr)
    : symbolTableManager(symTabMgr), 
      currentFunctionReturnType(DataType::VOID),
      loopDepth(0),
      hasError(false) {
}

SemanticAnalyzer::~SemanticAnalyzer() = default;

void SemanticAnalyzer::analyze(CompUnitNode* root) {
    root->accept(this);
}

void SemanticAnalyzer::reportError(const std::string& message) {
    std::cerr << "Semantic Error: " << message << std::endl;
    hasError = true;
}

void SemanticAnalyzer::checkType(ExpNode* exp, DataType expectedType) {
    if (!exp) {
        return;
    }
    
    if (auto* lval = dynamic_cast<LValNode*>(exp)) {
        VariableEntry* var = symbolTableManager->lookupVariable(lval->ident);
        if (var && var->isArray && lval->indices.empty()) {
            reportError("Cannot use array without subscript in expression: " + lval->ident);
            return;
        }

        if (var && var->isArray && lval->indices.size() < var->getDimensionCount()) {
            reportError("Array subscript results in array type, expected scalar: " + lval->ident);
            return;
        }
    }

    if (auto* funcCall = dynamic_cast<FuncCallNode*>(exp)) {
        FunctionEntry* func = symbolTableManager->lookupFunction(funcCall->ident);
        if (func && func->getReturnType() == DataType::VOID) {
            reportError("Cannot use void function in expression: " + funcCall->ident);
            return;
        }
        
        if (func && expectedType != DataType::VOID && func->getReturnType() != expectedType) {
            reportError("Type mismatch in function call: " + funcCall->ident);
        }
    }
}

int SemanticAnalyzer::evaluateConstExp(ConstExpNode* node) {
    return evaluateAddExp(node->addExp.get());
}

int SemanticAnalyzer::evaluateAddExp(AddExpNode* node) {
    if (!node->left) {
        return evaluateMulExp(node->right.get());
    }
    
    int leftVal = evaluateAddExp(node->left.get());
    int rightVal = evaluateMulExp(node->right.get());
    
    if (node->op == BinaryOp::PLUS) {
        return leftVal + rightVal;
    } else {
        return leftVal - rightVal;
    }
}

int SemanticAnalyzer::evaluateMulExp(MulExpNode* node) {
    if (!node->left) {
        return evaluateUnaryExp(node->right.get());
    }
    
    int leftVal = evaluateMulExp(node->left.get());
    int rightVal = evaluateUnaryExp(node->right.get());
    
    if (node->op == BinaryOp::MUL) {
        return leftVal * rightVal;
    } else if (node->op == BinaryOp::DIV) {
        if (rightVal == 0) {
            reportError("Division by zero in constant expression");
            return 0;
        }
        return leftVal / rightVal;
    } else {
        if (rightVal == 0) {
            reportError("Modulo by zero in constant expression");
            return 0;
        }
        return leftVal % rightVal;
    }
}

int SemanticAnalyzer::evaluateUnaryExp(UnaryExpNode* node) {
    if (node->operand) {
        int val = evaluateUnaryExp(node->operand.get());
        
        if (node->op == UnaryOp::PLUS) {
            return val;
        } else if (node->op == UnaryOp::MINUS) {
            return -val;
        } else {
            return !val;
        }
    }
    
    // TODO: 处理 PrimaryExp 的情况
    reportError("Invalid unary expression in constant context");
    return 0;
}

int SemanticAnalyzer::evaluatePrimaryExp(PrimaryExpNode* node) {
    if (node->type == PrimaryExpType::NUMBER) {
        return node->number->value;
    } else if (node->type == PrimaryExpType::LVAL) {
        return evaluateLVal(node->lVal.get());
    } else {
        // TODO: 处理括号表达式
        reportError("Cannot evaluate parenthesized expression in constant context");
        return 0;
    }
}

int SemanticAnalyzer::evaluateLVal(LValNode* node) {
    VariableEntry* var = symbolTableManager->lookupVariable(node->ident);
    if (!var) {
        reportError("Undefined variable: " + node->ident);
        return 0;
    }
    
    if (!var->isConst) {
        reportError("Variable " + node->ident + " is not a constant");
        return 0;
    }
    
    // TODO: 需要在 VariableEntry 中存储常量值
    reportError("Constant value evaluation not fully implemented");
    return 0;
}

void SemanticAnalyzer::visitCompUnit(CompUnitNode* node) {
    for (auto& decl : node->decls) {
        decl->accept(this);
    }
    
    for (auto& funcDef : node->funcDefs) {
        funcDef->accept(this);
    }
    
    FunctionEntry* mainFunc = symbolTableManager->lookupFunction("main");
    if (!mainFunc) {
        reportError("Missing main function");
    }
}

void SemanticAnalyzer::visitConstDecl(ConstDeclNode* node) {
    for (auto& constDef : node->constDefs) {
        constDef->accept(this);
    }
}

void SemanticAnalyzer::visitVarDecl(VarDeclNode* node) {
    for (auto& varDef : node->varDefs) {
        varDef->accept(this);
    }
}

void SemanticAnalyzer::visitConstDef(ConstDefNode* node) {
    if (symbolTableManager->getCurrentScope()->contains(node->ident)) {
        reportError("Redefinition of constant: " + node->ident);
        return;
    }
    
    auto entry = std::make_unique<VariableEntry>(
        node->ident, 
        DataType::INT,
        true,
        symbolTableManager->getCurrentScope()->getScopeLevel()
    );
    
    if (!node->dims.empty()) {
        entry->isArray = true;
        for (auto& dim : node->dims) {
            int dimSize = evaluateConstExp(dim.get());
            if (dimSize <= 0) {
                reportError("Array dimension must be positive: " + node->ident);
                dimSize = 1;
            }
            entry->dimensions.push_back(dimSize);
        }
        entry->dataType = DataType::INT_ARRAY;
    }
    
    if (node->initVal) {
        node->initVal->accept(this);
        entry->isInitialized = true;
    } else {
        reportError("Constant must be initialized: " + node->ident);
    }
    
    if (!symbolTableManager->insertLocal(std::move(entry))) {
        reportError("Failed to insert constant: " + node->ident);
    }
}

void SemanticAnalyzer::visitVarDef(VarDefNode* node) {
    if (symbolTableManager->getCurrentScope()->contains(node->ident)) {
        reportError("Redefinition of variable: " + node->ident);
        return;
    }
    
    auto entry = std::make_unique<VariableEntry>(
        node->ident, 
        DataType::INT,
        false,
        symbolTableManager->getCurrentScope()->getScopeLevel()
    );
    
    if (!node->dims.empty()) {
        entry->isArray = true;
        for (auto& dim : node->dims) {
            int dimSize = evaluateConstExp(dim.get());
            if (dimSize <= 0) {
                reportError("Array dimension must be positive: " + node->ident);
                dimSize = 1;
            }
            entry->dimensions.push_back(dimSize);
        }
        entry->dataType = DataType::INT_ARRAY;
    }
    
    if (node->initVal) {
        node->initVal->accept(this);
        entry->isInitialized = true;
    }
    
    if (!symbolTableManager->insertLocal(std::move(entry))) {
        reportError("Failed to insert variable: " + node->ident);
    }
}

void SemanticAnalyzer::visitConstInitVal(ConstInitValNode* node) {
    if (node->isScalar) {
        if (node->scalarVal) {
            node->scalarVal->accept(this);
        }
    } else {
        for (auto& val : node->arrayVals) {
            val->accept(this);
        }
    }
}

void SemanticAnalyzer::visitInitVal(InitValNode* node) {
    if (node->isScalar) {
        if (node->scalarVal) {
            node->scalarVal->accept(this);
        }
    } else {
        for (auto& val : node->arrayVals) {
            val->accept(this);
        }
    }
}

void SemanticAnalyzer::visitFuncDef(FuncDefNode* node) {
    if (symbolTableManager->lookupFunction(node->ident)) {
        reportError("Redefinition of function: " + node->ident);
        return;
    }
    
    DataType returnType = (node->funcType == FuncType::INT) ? DataType::INT : DataType::VOID;
    
    auto funcEntry = std::make_unique<FunctionEntry>(
        node->ident,
        returnType,
        0
    );
    
    FunctionEntry* funcPtr = funcEntry.get();
    if (!symbolTableManager->insertFunction(std::move(funcEntry))) {
        reportError("Failed to insert function: " + node->ident);
        return;
    }
    
    symbolTableManager->enterFunction(funcPtr);
    currentFunctionReturnType = returnType;
    
    for (auto& param : node->params) {
        param->accept(this);
    }
    
    if (node->block) {
        node->block->accept(this);
    }
    
    symbolTableManager->exitFunction();
}

void SemanticAnalyzer::visitFuncFParam(FuncFParamNode* node) {
    if (symbolTableManager->getCurrentScope()->contains(node->ident)) {
        reportError("Redefinition of parameter: " + node->ident);
        return;
    }
    
    // TODO: 需要从外部传入 paramIndex
    auto paramEntry = std::make_unique<ParameterEntry>(
        node->ident,
        node->isArray ? DataType::INT_ARRAY : DataType::INT,
        symbolTableManager->getCurrentScope()->getScopeLevel(),
        0
    );
    
    paramEntry->isArrayParam = node->isArray;
    paramEntry->isArray = node->isArray;
    
    if (node->isArray && !node->dims.empty()) {
        for (auto& dim : node->dims) {
            if (dim) {
                dim->accept(this);
            }
        }
    }
    
    if (!symbolTableManager->insertParameter(std::move(paramEntry))) {
        reportError("Failed to insert parameter: " + node->ident);
    }
}

void SemanticAnalyzer::visitBlockStmt(BlockStmtNode* node) {
    // TODO: 优化函数体第一个块的作用域处理
    bool isFirstBlockInFunc = (symbolTableManager->getCurrentScope()->getScopeLevel() == 1 &&
                                symbolTableManager->getCurrentScope()->getParent() == symbolTableManager->getGlobalScope());
    
    if (!isFirstBlockInFunc) {
        symbolTableManager->enterScope();
    }
    
    for (auto& item : node->blockItems) {
        item->accept(this);
    }
    
    if (!isFirstBlockInFunc) {
        symbolTableManager->exitScope();
    }
}

void SemanticAnalyzer::visitBlockItem(BlockItemNode* node) {
    if (node->isDecl) {
        if (node->decl) {
            node->decl->accept(this);
        }
    } else {
        if (node->stmt) {
            node->stmt->accept(this);
        }
    }
}

void SemanticAnalyzer::visitAssignStmt(AssignStmtNode* node) {
    if (node->lVal) {
        node->lVal->accept(this);
        
        VariableEntry* var = symbolTableManager->lookupVariable(node->lVal->ident);
        if (var && var->isConst) {
            reportError("Cannot assign to constant: " + node->lVal->ident);
        }
    }
    
    if (node->exp) {
        node->exp->accept(this);
    }
}

void SemanticAnalyzer::visitExpStmt(ExpStmtNode* node) {
    if (node->exp) {
        node->exp->accept(this);
    }
}

void SemanticAnalyzer::visitIfStmt(IfStmtNode* node) {
    if (node->cond) {
        node->cond->accept(this);
    }
    if (node->thenStmt) {
        node->thenStmt->accept(this);
    }
    if (node->elseStmt) {
        node->elseStmt->accept(this);
    }
}

void SemanticAnalyzer::visitWhileStmt(WhileStmtNode* node) {
    loopDepth++;
    
    if (node->cond) {
        node->cond->accept(this);
    }
    if (node->body) {
        node->body->accept(this);
    }
    loopDepth--;
}

void SemanticAnalyzer::visitBreakStmt(BreakStmtNode* node) {
    if (loopDepth == 0) {
        reportError("break statement not within loop");
    }
}

void SemanticAnalyzer::visitContinueStmt(ContinueStmtNode* node) {
    if (loopDepth == 0) {
        reportError("continue statement not within loop");
    }
}

void SemanticAnalyzer::visitReturnStmt(ReturnStmtNode* node) {
    if (node->exp) {
        node->exp->accept(this);
        
        if (currentFunctionReturnType == DataType::VOID) {
            reportError("void function should not return a value");
        }
    } else {
        if (currentFunctionReturnType != DataType::VOID) {
            reportError("non-void function must return a value");
        }
    }
}

void SemanticAnalyzer::visitAddExp(AddExpNode* node) {
    if (node->left) {
        node->left->accept(this);
    }
    if (node->right) {
        node->right->accept(this);
    }
}

void SemanticAnalyzer::visitMulExp(MulExpNode* node) {
    if (node->left) {
        node->left->accept(this);
    }
    
    if (node->right) {
        node->right->accept(this);
    }
}

void SemanticAnalyzer::visitUnaryExp(UnaryExpNode* node) {
    if (node->operand) {
        node->operand->accept(this);
    }
}

void SemanticAnalyzer::visitPrimaryExp(PrimaryExpNode* node) {
    if (node->type == PrimaryExpType::NUMBER) {
        if (node->number) {
            node->number->accept(this);
        }
    } else if (node->type == PrimaryExpType::LVAL) {
        if (node->lVal) {
            node->lVal->accept(this);
        }
    } else {  // PAREN_EXP
        if (node->exp) {
            node->exp->accept(this);
        }
    }
}

void SemanticAnalyzer::visitLVal(LValNode* node) {
    VariableEntry* var = symbolTableManager->lookupVariable(node->ident);
    if (!var) {
        reportError("Undefined variable: " + node->ident);
        return;
    }

    if (!node->indices.empty()) {
        if (!var->isArray) {
            reportError("Subscripted value is not an array: " + node->ident);
            return;
        }

        for (auto& index : node->indices) {
            index->accept(this);
        }

        if (node->indices.size() > var->getDimensionCount()) {
            reportError("Too many subscripts for array: " + node->ident);
        }
    }
}

void SemanticAnalyzer::visitNumber(NumberNode* node) {
    if (node->value < 0) {
        reportError("Integer literal overflow: " + std::to_string(node->value));
    }
    
}

void SemanticAnalyzer::visitFuncCall(FuncCallNode* node) {
    FunctionEntry* func = symbolTableManager->lookupFunction(node->ident);
    if (!func) {
        reportError("Undefined function: " + node->ident);
        return;
    }

    if (node->args.size() != func->getParameterCount()) {
        reportError("Function " + node->ident + " expects " + 
                   std::to_string(func->getParameterCount()) + " arguments, but got " + 
                   std::to_string(node->args.size()));
    }

    for (auto& arg : node->args) {
        arg->accept(this);
    }

}

void SemanticAnalyzer::visitRelExp(RelExpNode* node) {
    if (node->left) {
        node->left->accept(this);
    }
    if (node->right) {
        node->right->accept(this);
    }
}

void SemanticAnalyzer::visitEqExp(EqExpNode* node) {
    if (node->left) {
        node->left->accept(this);
    }
    if (node->right) {
        node->right->accept(this);
    }
}

void SemanticAnalyzer::visitLAndExp(LAndExpNode* node) {
    if (node->left) {
        node->left->accept(this);
    }
    if (node->right) {
        node->right->accept(this);
    }
}

void SemanticAnalyzer::visitLOrExp(LOrExpNode* node) {
    if (node->left) {
        node->left->accept(this);
    }
    if (node->right) {
        node->right->accept(this);
    }
}

void SemanticAnalyzer::visitCond(CondNode* node) {
    if (node->lOrExp) {
        node->lOrExp->accept(this);
    }
}

void SemanticAnalyzer::visitConstExp(ConstExpNode* node) {
    if (node->addExp) {
        node->addExp->accept(this);
    }
}

} // namespace sysy
