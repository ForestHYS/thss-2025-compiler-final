#include "CodeGenerator.h"
#include "Nodes.h"
#include <sstream>
#include <map>

namespace sysy
{
    // 用于存储表达式结果的临时变量计数器
    static int tempCounter = 0;

    // 用于存储当前表达式的结果
    static std::string currentValue;

    // 用于存储条件表达式的结果寄存器
    static std::string condResultReg;

    CodeGenerator::CodeGenerator(SymbolTableManager *symTabMgr)
        : symbolTableManager(symTabMgr), labelCounter(0), currentParamIndex(0)
    {
    }

    CodeGenerator::~CodeGenerator() = default;

    std::string CodeGenerator::generate(CompUnitNode *root)
    {
        irStream.str(""); // 清空流
        irStream.clear();
        tempCounter = 0; // 重置临时变量计数器

        // 生成模块头部
        irStream << "; ModuleID = 'sysy_module'\n";
        irStream << "source_filename = \"sysy\"\n\n";

        root->accept(this);

        return irStream.str();
    }

    std::string CodeGenerator::getLLVMType(DataType dataType)
    {
        switch (dataType)
        {
        case DataType::INT:
            return "i32";
        case DataType::VOID:
            return "void";
        case DataType::INT_ARRAY:
            return "i32*"; // 数组作为指针
        default:
            return "i32";
        }
    }

    std::string CodeGenerator::getLLVMArrayType(DataType elementType, const std::vector<int> &dimensions)
    {
        std::string baseType = getLLVMType(elementType);
        // 从内到外构建数组类型，例如 [2 x [3 x i32]]
        for (auto it = dimensions.rbegin(); it != dimensions.rend(); ++it)
        {
            baseType = "[" + std::to_string(*it) + " x " + baseType + "]";
        }
        return baseType;
    }

    std::string CodeGenerator::generateLabel(const std::string &prefix)
    {
        return prefix + std::to_string(labelCounter++);
    }

    std::string CodeGenerator::generateVarName(const std::string &name)
    {
        // 处理变量名，确保符合LLVM IR命名规范
        return "%" + name;
    }

    // 实现所有visit方法（框架代码，不做实际工作）
    void CodeGenerator::visitCompUnit(CompUnitNode *node)
    {
        // 遍历所有全局声明
        for (auto &decl : node->decls)
        {
            decl->accept(this);
        }

        // 遍历所有函数定义
        for (auto &funcDef : node->funcDefs)
        {
            funcDef->accept(this);
        }
    }

    void CodeGenerator::visitConstDecl(ConstDeclNode *node)
    {
        // 遍历所有常量定义
        for (auto &constDef : node->constDefs)
        {
            constDef->accept(this);
        }
    }

    void CodeGenerator::visitVarDecl(VarDeclNode *node)
    {
        // 遍历所有变量定义
        for (auto &varDef : node->varDefs)
        {
            varDef->accept(this);
        }
    }

    void CodeGenerator::visitConstDef(ConstDefNode *node)
    {
        // 查找符号表条目
        SymbolEntry *entry = symbolTableManager->lookup(node->ident);
        if (!entry)
            return;

        VariableEntry *constEntry = dynamic_cast<VariableEntry *>(entry);
        if (!constEntry)
            return;

        // 判断是全局还是局部常量
        if (constEntry->getScopeLevel() == 0)
        {
            // 全局常量 - 生成为全局常量
            std::string constName = "@" + node->ident;
            constEntry->irName = constName;

            if (constEntry->isArray)
            {
                // 全局常量数组 - 生成为 constant
                std::string arrayType = getLLVMArrayType(DataType::INT, constEntry->dimensions);
                // 简化处理：使用 zeroinitializer，实际应该根据 initVal 生成具体的初始值
                irStream << constName << " = constant " << arrayType << " zeroinitializer\n";
            }
            else
            {
                // 全局常量标量
                int initValue = 0;
                if (node->initVal && node->initVal->isScalar && node->initVal->scalarVal)
                {
                    // 计算常量初始值
                    node->initVal->scalarVal->accept(this);
                    initValue = std::stoi(currentValue);
                }
                irStream << constName << " = constant i32 " << initValue << "\n";
            }
        }
        else
        {
            // 局部常量
            // 对于局部常量，可以选择生成 alloca 并标记为只读
            // 或者完全通过符号表内联（当前采用生成 alloca 的方式）
            std::string constName = "%" + node->ident;
            constEntry->irName = constName;

            if (constEntry->isArray)
            {
                // 局部常量数组
                std::string arrayType = getLLVMArrayType(DataType::INT, constEntry->dimensions);
                irStream << "  " << constName << " = alloca " << arrayType << "\n";
                // 可以添加初始化逻辑
            }
            else
            {
                // 局部常量标量 - 生成 alloca 并初始化
                irStream << "  " << constName << " = alloca i32\n";

                if (node->initVal && node->initVal->isScalar && node->initVal->scalarVal)
                {
                    node->initVal->scalarVal->accept(this);
                    irStream << "  store i32 " << currentValue << ", i32* " << constName << "\n";
                }
            }
        }
    }

    void CodeGenerator::visitVarDef(VarDefNode *node)
    {
        // 查找符号表条目
        SymbolEntry *entry = symbolTableManager->lookup(node->ident);
        if (!entry)
            return;

        VariableEntry *varEntry = dynamic_cast<VariableEntry *>(entry);
        if (!varEntry)
            return;

        // 判断是全局还是局部变量
        if (varEntry->getScopeLevel() == 0)
        {
            // 全局变量
            std::string varName = "@" + node->ident;
            varEntry->irName = varName;

            if (varEntry->isArray)
            {
                // 全局数组
                std::string arrayType = getLLVMArrayType(DataType::INT, varEntry->dimensions);
                irStream << varName << " = global " << arrayType << " zeroinitializer\n";
            }
            else
            {
                // 全局标量
                int initValue = 0;
                if (node->initVal)
                {
                    // 如果有初始值，需要求值（简化处理，假设为0）
                    // 实际应该执行常量求值
                }
                irStream << varName << " = global i32 " << initValue << "\n";
            }
        }
        else
        {
            // 局部变量 - 生成 alloca 指令
            std::string varName = "%" + node->ident;
            varEntry->irName = varName;

            if (varEntry->isArray)
            {
                // 局部数组
                std::string arrayType = getLLVMArrayType(DataType::INT, varEntry->dimensions);
                irStream << "  " << varName << " = alloca " << arrayType << "\n";
            }
            else
            {
                // 局部标量
                irStream << "  " << varName << " = alloca i32\n";

                // 如果有初始值，生成 store 指令
                if (node->initVal)
                {
                    node->initVal->accept(this);
                    irStream << "  store i32 " << currentValue << ", i32* " << varName << "\n";
                }
            }
        }
    }

    void CodeGenerator::visitConstInitVal(ConstInitValNode *node)
    {
        // 常量初始化值通常在编译时处理，不需要生成运行时代码
        if (node->isScalar && node->scalarVal)
        {
            node->scalarVal->accept(this);
        }
    }

    void CodeGenerator::visitInitVal(InitValNode *node)
    {
        if (node->isScalar && node->scalarVal)
        {
            node->scalarVal->accept(this);
        }
        else
        {
            // 数组初始化 - 简化处理
            for (auto &val : node->arrayVals)
            {
                val->accept(this);
            }
        }
    }

    void CodeGenerator::visitFuncDef(FuncDefNode *node)
    {
        // 查找函数符号表条目
        FunctionEntry *funcEntry = symbolTableManager->lookupFunction(node->ident);
        if (!funcEntry)
            return;

        // 生成函数签名
        std::string funcName = "@" + node->ident;
        funcEntry->irName = funcName;

        // 返回类型
        std::string retType = (node->funcType == FuncType::VOID) ? "void" : "i32";

        // 参数列表
        irStream << "\ndefine " << retType << " " << funcName << "(";
        for (size_t i = 0; i < node->params.size(); ++i)
        {
            if (i > 0)
                irStream << ", ";

            // 根据参数类型生成对应的LLVM类型
            if (node->params[i]->isArray)
            {
                irStream << "i32* %arg" << i;
            }
            else
            {
                irStream << "i32 %arg" << i;
            }
        }
        irStream << ") {\n";

        // 为每个参数调用visitFuncFParam处理
        currentParamIndex = 0;
        for (size_t i = 0; i < node->params.size(); ++i)
        {
            currentParamIndex = i;
            node->params[i]->accept(this);
        }

        // 生成函数体
        bool hasReturn = false;
        if (node->block)
        {
            // 遍历块项目，但不生成额外的大括号
            for (auto &item : node->block->blockItems)
            {
                item->accept(this);
                // 检查最后一个语句是否为 return（简化判断）
                if (item->stmt && dynamic_cast<ReturnStmtNode *>(item->stmt.get()))
                {
                    hasReturn = true;
                }
            }
        }

        // 如果函数末尾没有 return 语句，添加默认 return
        if (!hasReturn)
        {
            if (node->funcType == FuncType::VOID)
            {
                irStream << "  ret void\n";
            }
            else
            {
                // int 函数如果没有 return，添加 ret i32 0（符合 C 标准）
                irStream << "  ret i32 0\n";
            }
        }

        irStream << "}\n";
    }

    void CodeGenerator::visitFuncFParam(FuncFParamNode *node)
    {
        // 查找符号表条目
        SymbolEntry *paramEntry = symbolTableManager->lookup(node->ident);
        if (!paramEntry)
            return;

        VariableEntry *varEntry = dynamic_cast<VariableEntry *>(paramEntry);
        if (!varEntry)
            return;

        // 生成参数的局部变量名
        std::string paramName = "%" + node->ident;
        varEntry->irName = paramName;

        if (node->isArray)
        {
            // 数组参数 - 分配指针空间并存储传入的数组指针
            irStream << "  " << paramName << " = alloca i32*\n";
            irStream << "  store i32* %arg" << currentParamIndex << ", i32** " << paramName << "\n";
        }
        else
        {
            // 标量参数 - 分配空间并存储参数值
            irStream << "  " << paramName << " = alloca i32\n";
            irStream << "  store i32 %arg" << currentParamIndex << ", i32* " << paramName << "\n";
        }
    }

    void CodeGenerator::visitBlockStmt(BlockStmtNode *node)
    {
        // 遍历所有块项目
        for (auto &item : node->blockItems)
        {
            item->accept(this);
        }
    }

    void CodeGenerator::visitBlockItem(BlockItemNode *node)
    {
        if (node->isDecl && node->decl)
        {
            node->decl->accept(this);
        }
        else if (node->stmt)
        {
            node->stmt->accept(this);
        }
    }

    void CodeGenerator::visitAssignStmt(AssignStmtNode *node)
    {
        // 计算右值
        if (node->exp)
        {
            node->exp->accept(this);
            std::string rhsValue = currentValue;

            // 获取左值地址
            SymbolEntry *entry = symbolTableManager->lookup(node->lVal->ident);
            if (!entry)
                return;

            VariableEntry *varEntry = dynamic_cast<VariableEntry *>(entry);
            if (!varEntry)
                return;

            if (node->lVal->indices.empty())
            {
                // 标量赋值
                irStream << "  store i32 " << rhsValue << ", i32* " << varEntry->irName << "\n";
            }
            else
            {
                // 数组元素赋值
                // 计算所有索引表达式
                std::vector<std::string> indexValues;
                for (auto &index : node->lVal->indices)
                {
                    index->accept(this);
                    indexValues.push_back(currentValue);
                }

                // 生成 getelementptr 指令获取数组元素地址
                std::string ptrReg = "%t" + std::to_string(tempCounter++);

                if (varEntry->getScopeLevel() == 0)
                {
                    // 全局数组
                    irStream << "  " << ptrReg << " = getelementptr ";
                    irStream << getLLVMArrayType(DataType::INT, varEntry->dimensions);
                    irStream << ", " << getLLVMArrayType(DataType::INT, varEntry->dimensions) << "* ";
                    irStream << varEntry->irName;

                    // 第一个索引是0（访问数组本身）
                    irStream << ", i32 0";

                    // 后续索引
                    for (const auto &idx : indexValues)
                    {
                        irStream << ", i32 " << idx;
                    }
                    irStream << "\n";
                }
                else
                {
                    // 局部数组或数组参数
                    if (varEntry->isArray && indexValues.size() > 0)
                    {
                        // 对于局部数组，需要先load数组基地址（如果是数组参数）
                        // 或者直接使用 getelementptr（如果是局部数组）

                        // 检查是否为数组参数（通过irName中是否存储了指针）
                        std::string basePtr = varEntry->irName;

                        // 生成 getelementptr 指令
                        irStream << "  " << ptrReg << " = getelementptr ";
                        irStream << getLLVMArrayType(DataType::INT, varEntry->dimensions);
                        irStream << ", " << getLLVMArrayType(DataType::INT, varEntry->dimensions) << "* ";
                        irStream << basePtr;

                        // 第一个索引是0
                        irStream << ", i32 0";

                        // 后续索引
                        for (const auto &idx : indexValues)
                        {
                            irStream << ", i32 " << idx;
                        }
                        irStream << "\n";
                    }
                }

                // 存储值到数组元素
                irStream << "  store i32 " << rhsValue << ", i32* " << ptrReg << "\n";
            }
        }
    }

    void CodeGenerator::visitExpStmt(ExpStmtNode *node)
    {
        if (node->exp)
        {
            node->exp->accept(this);
        }
    }

    void CodeGenerator::visitIfStmt(IfStmtNode *node)
    {
        // 生成标签
        std::string thenLabel = generateLabel("if.then");
        std::string elseLabel = generateLabel("if.else");
        std::string endLabel = generateLabel("if.end");

        // 生成条件表达式
        if (node->cond)
        {
            node->cond->accept(this);
            std::string condReg = condResultReg;

            // 条件跳转
            if (node->elseStmt)
            {
                irStream << "  br i1 " << condReg << ", label %" << thenLabel
                         << ", label %" << elseLabel << "\n";
            }
            else
            {
                irStream << "  br i1 " << condReg << ", label %" << thenLabel
                         << ", label %" << endLabel << "\n";
            }
        }

        // then分支
        irStream << thenLabel << ":\n";
        if (node->thenStmt)
        {
            node->thenStmt->accept(this);
        }
        irStream << "  br label %" << endLabel << "\n";

        // else分支
        if (node->elseStmt)
        {
            irStream << elseLabel << ":\n";
            node->elseStmt->accept(this);
            irStream << "  br label %" << endLabel << "\n";
        }

        // 结束标签
        irStream << endLabel << ":\n";
    }

    void CodeGenerator::visitWhileStmt(WhileStmtNode *node)
    {
        // 保存之前的break/continue目标
        std::string oldBreak = currentBreakTarget;
        std::string oldContinue = currentContinueTarget;

        // 生成标签
        std::string condLabel = generateLabel("while.cond");
        std::string bodyLabel = generateLabel("while.body");
        std::string endLabel = generateLabel("while.end");

        // 设置当前break/continue目标
        currentBreakTarget = endLabel;
        currentContinueTarget = condLabel;

        // 跳转到条件判断
        irStream << "  br label %" << condLabel << "\n";

        // 条件判断块
        irStream << condLabel << ":\n";
        if (node->cond)
        {
            node->cond->accept(this);
            std::string condReg = condResultReg;
            irStream << "  br i1 " << condReg << ", label %" << bodyLabel
                     << ", label %" << endLabel << "\n";
        }

        // 循环体
        irStream << bodyLabel << ":\n";
        if (node->body)
        {
            node->body->accept(this);
        }
        irStream << "  br label %" << condLabel << "\n";

        // 结束标签
        irStream << endLabel << ":\n";

        // 恢复之前的break/continue目标
        currentBreakTarget = oldBreak;
        currentContinueTarget = oldContinue;
    }

    void CodeGenerator::visitBreakStmt(BreakStmtNode *node)
    {
        if (!currentBreakTarget.empty())
        {
            irStream << "  br label %" << currentBreakTarget << "\n";
        }
    }

    void CodeGenerator::visitContinueStmt(ContinueStmtNode *node)
    {
        if (!currentContinueTarget.empty())
        {
            irStream << "  br label %" << currentContinueTarget << "\n";
        }
    }

    void CodeGenerator::visitReturnStmt(ReturnStmtNode *node)
    {
        if (node->exp)
        {
            node->exp->accept(this);
            irStream << "  ret i32 " << currentValue << "\n";
        }
        else
        {
            irStream << "  ret void\n";
        }
    }

    void CodeGenerator::visitAddExp(AddExpNode *node)
    {
        if (!node->left)
        {
            // 单操作数，直接访问右操作数
            node->right->accept(this);
        }
        else
        {
            // 二元操作
            node->left->accept(this);
            std::string leftVal = currentValue;

            node->right->accept(this);
            std::string rightVal = currentValue;

            std::string resultReg = "%t" + std::to_string(tempCounter++);

            if (node->op == BinaryOp::PLUS)
            {
                irStream << "  " << resultReg << " = add i32 " << leftVal << ", " << rightVal << "\n";
            }
            else if (node->op == BinaryOp::MINUS)
            {
                irStream << "  " << resultReg << " = sub i32 " << leftVal << ", " << rightVal << "\n";
            }

            currentValue = resultReg;
        }
    }

    void CodeGenerator::visitMulExp(MulExpNode *node)
    {
        if (!node->left)
        {
            // 单操作数，直接访问右操作数
            node->right->accept(this);
        }
        else
        {
            // 二元操作
            node->left->accept(this);
            std::string leftVal = currentValue;

            node->right->accept(this);
            std::string rightVal = currentValue;

            std::string resultReg = "%t" + std::to_string(tempCounter++);

            if (node->op == BinaryOp::MUL)
            {
                irStream << "  " << resultReg << " = mul i32 " << leftVal << ", " << rightVal << "\n";
            }
            else if (node->op == BinaryOp::DIV)
            {
                irStream << "  " << resultReg << " = sdiv i32 " << leftVal << ", " << rightVal << "\n";
            }
            else if (node->op == BinaryOp::MOD)
            {
                irStream << "  " << resultReg << " = srem i32 " << leftVal << ", " << rightVal << "\n";
            }

            currentValue = resultReg;
        }
    }

    void CodeGenerator::visitUnaryExp(UnaryExpNode *node)
    {
        if (node->operand)
        {
            // 递归处理操作数
            node->operand->accept(this);
            std::string operandVal = currentValue;

            if (node->op == UnaryOp::PLUS)
            {
                // 正号不需要操作
                currentValue = operandVal;
            }
            else if (node->op == UnaryOp::MINUS)
            {
                // 负号：0 - operand
                std::string resultReg = "%t" + std::to_string(tempCounter++);
                irStream << "  " << resultReg << " = sub i32 0, " << operandVal << "\n";
                currentValue = resultReg;
            }
            else if (node->op == UnaryOp::NOT)
            {
                // 逻辑非：icmp eq operand, 0
                std::string resultReg = "%t" + std::to_string(tempCounter++);
                irStream << "  " << resultReg << " = icmp eq i32 " << operandVal << ", 0\n";
                // 将i1转换为i32
                std::string extReg = "%t" + std::to_string(tempCounter++);
                irStream << "  " << extReg << " = zext i1 " << resultReg << " to i32\n";
                currentValue = extReg;
            }
        }
    }

    void CodeGenerator::visitPrimaryExp(PrimaryExpNode *node)
    {
        if (node->type == PrimaryExpType::NUMBER && node->number)
        {
            node->number->accept(this);
        }
        else if (node->type == PrimaryExpType::LVAL && node->lVal)
        {
            node->lVal->accept(this);
        }
        else if (node->type == PrimaryExpType::PAREN_EXP && node->exp)
        {
            node->exp->accept(this);
        }
    }

    void CodeGenerator::visitLVal(LValNode *node)
    {
        // 查找符号表条目
        SymbolEntry *entry = symbolTableManager->lookup(node->ident);
        if (!entry)
            return;

        VariableEntry *varEntry = dynamic_cast<VariableEntry *>(entry);
        if (!varEntry)
            return;

        if (node->indices.empty())
        {
            // 标量变量，生成load指令
            std::string resultReg = "%t" + std::to_string(tempCounter++);
            irStream << "  " << resultReg << " = load i32, i32* " << varEntry->irName << "\n";
            currentValue = resultReg;
        }
        else
        {
            // 数组元素访问
            // 计算所有索引表达式
            std::vector<std::string> indexValues;
            for (auto &index : node->indices)
            {
                index->accept(this);
                indexValues.push_back(currentValue);
            }

            // 生成 getelementptr 指令获取数组元素地址
            std::string ptrReg = "%t" + std::to_string(tempCounter++);

            if (varEntry->getScopeLevel() == 0)
            {
                // 全局数组
                irStream << "  " << ptrReg << " = getelementptr ";
                irStream << getLLVMArrayType(DataType::INT, varEntry->dimensions);
                irStream << ", " << getLLVMArrayType(DataType::INT, varEntry->dimensions) << "* ";
                irStream << varEntry->irName;

                // 第一个索引是0（访问数组本身）
                irStream << ", i32 0";

                // 后续索引
                for (const auto &idx : indexValues)
                {
                    irStream << ", i32 " << idx;
                }
                irStream << "\n";
            }
            else
            {
                // 局部数组
                irStream << "  " << ptrReg << " = getelementptr ";
                irStream << getLLVMArrayType(DataType::INT, varEntry->dimensions);
                irStream << ", " << getLLVMArrayType(DataType::INT, varEntry->dimensions) << "* ";
                irStream << varEntry->irName;

                // 第一个索引是0
                irStream << ", i32 0";

                // 后续索引
                for (const auto &idx : indexValues)
                {
                    irStream << ", i32 " << idx;
                }
                irStream << "\n";
            }

            // 从数组元素地址加载值
            std::string resultReg = "%t" + std::to_string(tempCounter++);
            irStream << "  " << resultReg << " = load i32, i32* " << ptrReg << "\n";
            currentValue = resultReg;
        }
    }

    void CodeGenerator::visitNumber(NumberNode *node)
    {
        currentValue = std::to_string(node->value);
    }

    void CodeGenerator::visitFuncCall(FuncCallNode *node)
    {
        // 查找函数
        FunctionEntry *funcEntry = symbolTableManager->lookupFunction(node->ident);
        if (!funcEntry)
            return;

        // 计算所有参数
        std::vector<std::string> argValues;
        for (auto &arg : node->args)
        {
            arg->accept(this);
            argValues.push_back(currentValue);
        }

        // 生成调用指令
        std::string funcName = "@" + node->ident;

        if (funcEntry->getReturnType() == DataType::VOID)
        {
            irStream << "  call void " << funcName << "(";
            for (size_t i = 0; i < argValues.size(); ++i)
            {
                if (i > 0)
                    irStream << ", ";
                irStream << "i32 " << argValues[i];
            }
            irStream << ")\n";
            currentValue = "";
        }
        else
        {
            std::string resultReg = "%t" + std::to_string(tempCounter++);
            irStream << "  " << resultReg << " = call i32 " << funcName << "(";
            for (size_t i = 0; i < argValues.size(); ++i)
            {
                if (i > 0)
                    irStream << ", ";
                irStream << "i32 " << argValues[i];
            }
            irStream << ")\n";
            currentValue = resultReg;
        }
    }

    void CodeGenerator::visitRelExp(RelExpNode *node)
    {
        if (!node->left)
        {
            // 单操作数
            node->right->accept(this);
        }
        else
        {
            // 二元操作
            node->left->accept(this);
            std::string leftVal = currentValue;

            node->right->accept(this);
            std::string rightVal = currentValue;

            std::string cmpReg = "%t" + std::to_string(tempCounter++);
            std::string cmpOp;

            switch (node->op)
            {
            case BinaryOp::LT:
                cmpOp = "slt";
                break;
            case BinaryOp::GT:
                cmpOp = "sgt";
                break;
            case BinaryOp::LE:
                cmpOp = "sle";
                break;
            case BinaryOp::GE:
                cmpOp = "sge";
                break;
            default:
                break;
            }

            irStream << "  " << cmpReg << " = icmp " << cmpOp << " i32 " << leftVal << ", " << rightVal << "\n";

            // 将i1转换为i32
            std::string extReg = "%t" + std::to_string(tempCounter++);
            irStream << "  " << extReg << " = zext i1 " << cmpReg << " to i32\n";
            currentValue = extReg;
            condResultReg = cmpReg; // 保存i1结果用于条件分支
        }
    }

    void CodeGenerator::visitEqExp(EqExpNode *node)
    {
        if (!node->left)
        {
            // 单操作数
            node->right->accept(this);
        }
        else
        {
            // 二元操作
            node->left->accept(this);
            std::string leftVal = currentValue;

            node->right->accept(this);
            std::string rightVal = currentValue;

            std::string cmpReg = "%t" + std::to_string(tempCounter++);
            std::string cmpOp = (node->op == BinaryOp::EQ) ? "eq" : "ne";

            irStream << "  " << cmpReg << " = icmp " << cmpOp << " i32 " << leftVal << ", " << rightVal << "\n";

            // 将i1转换为i32
            std::string extReg = "%t" + std::to_string(tempCounter++);
            irStream << "  " << extReg << " = zext i1 " << cmpReg << " to i32\n";
            currentValue = extReg;
            condResultReg = cmpReg; // 保存i1结果用于条件分支
        }
    }

    void CodeGenerator::visitLAndExp(LAndExpNode *node)
    {
        if (!node->left)
        {
            // 单操作数
            node->right->accept(this);
            // 将结果转换为bool
            std::string cmpReg = "%t" + std::to_string(tempCounter++);
            irStream << "  " << cmpReg << " = icmp ne i32 " << currentValue << ", 0\n";
            condResultReg = cmpReg;
        }
        else
        {
            // 二元操作 - 短路求值
            node->left->accept(this);
            std::string leftVal = currentValue;

            // 将左操作数转换为bool
            std::string leftCmp = "%t" + std::to_string(tempCounter++);
            irStream << "  " << leftCmp << " = icmp ne i32 " << leftVal << ", 0\n";

            node->right->accept(this);
            std::string rightVal = currentValue;

            // 将右操作数转换为bool
            std::string rightCmp = "%t" + std::to_string(tempCounter++);
            irStream << "  " << rightCmp << " = icmp ne i32 " << rightVal << ", 0\n";

            // 逻辑与操作
            std::string andReg = "%t" + std::to_string(tempCounter++);
            irStream << "  " << andReg << " = and i1 " << leftCmp << ", " << rightCmp << "\n";

            // 转换为i32
            std::string extReg = "%t" + std::to_string(tempCounter++);
            irStream << "  " << extReg << " = zext i1 " << andReg << " to i32\n";
            currentValue = extReg;
            condResultReg = andReg;
        }
    }

    void CodeGenerator::visitLOrExp(LOrExpNode *node)
    {
        if (!node->left)
        {
            // 单操作数
            node->right->accept(this);
            // 将结果转换为bool
            std::string cmpReg = "%t" + std::to_string(tempCounter++);
            irStream << "  " << cmpReg << " = icmp ne i32 " << currentValue << ", 0\n";
            condResultReg = cmpReg;
        }
        else
        {
            // 二元操作 - 短路求值
            node->left->accept(this);
            std::string leftVal = currentValue;

            // 将左操作数转换为bool
            std::string leftCmp = "%t" + std::to_string(tempCounter++);
            irStream << "  " << leftCmp << " = icmp ne i32 " << leftVal << ", 0\n";

            node->right->accept(this);
            std::string rightVal = currentValue;

            // 将右操作数转换为bool
            std::string rightCmp = "%t" + std::to_string(tempCounter++);
            irStream << "  " << rightCmp << " = icmp ne i32 " << rightVal << ", 0\n";

            // 逻辑或操作
            std::string orReg = "%t" + std::to_string(tempCounter++);
            irStream << "  " << orReg << " = or i1 " << leftCmp << ", " << rightCmp << "\n";

            // 转换为i32
            std::string extReg = "%t" + std::to_string(tempCounter++);
            irStream << "  " << extReg << " = zext i1 " << orReg << " to i32\n";
            currentValue = extReg;
            condResultReg = orReg;
        }
    }

    void CodeGenerator::visitCond(CondNode *node)
    {
        if (node->lOrExp)
        {
            node->lOrExp->accept(this);
        }
    }

    void CodeGenerator::visitConstExp(ConstExpNode *node)
    {
        if (node->addExp)
        {
            node->addExp->accept(this);
        }
    }

} // namespace sysy
