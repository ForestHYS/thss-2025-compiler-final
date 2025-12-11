#include "CodeGenerator.h"
#include "Nodes.h"
#include <sstream>
#include <map>
#include <iostream>

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

        // 生成系统函数声明
        generateSystemFunctionDeclarations();

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
        // 尝试从符号表获取类型信息（可选，用于获取维度信息）
        SymbolEntry *entry = symbolTableManager->getCurrentScope()->lookup(node->ident);
        if (!entry) {
            entry = symbolTableManager->lookup(node->ident);
        }
        
        VariableEntry *constEntry = dynamic_cast<VariableEntry *>(entry);
        
        // 关键修复：基于当前代码生成上下文判断，而不是符号表中变量的 scopeLevel
        int currentScopeLevel = symbolTableManager->getCurrentScope()->getScopeLevel();
        bool isGlobal = (currentScopeLevel == 0);
        
        std::vector<int> dimensions;
        bool isArray = !node->dims.empty();
        
        if (constEntry) {
            // 从符号表获取维度信息
            dimensions = constEntry->dimensions;
            isArray = constEntry->isArray;
        }
        
        if (isGlobal)
        {
            // 全局常量 - 生成为全局常量
            std::string constName = "@" + node->ident;
            if (constEntry) {
                constEntry->irName = constName;
            }

            if (isArray && !dimensions.empty())
            {
                // 全局常量数组 - 生成为 constant
                std::string arrayType = getLLVMArrayType(DataType::INT, dimensions);
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
            if (constEntry) {
                constEntry->irName = constName;
            }

            if (isArray && !dimensions.empty())
            {
                // 局部常量数组
                std::string arrayType = getLLVMArrayType(DataType::INT, dimensions);
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
        // 尝试从符号表获取类型信息（可选，用于获取维度信息）
        SymbolEntry *entry = symbolTableManager->getCurrentScope()->lookup(node->ident);
        if (!entry) {
            entry = symbolTableManager->lookup(node->ident);
        }
        
        VariableEntry *varEntry = dynamic_cast<VariableEntry *>(entry);
        
        // 关键修复：基于当前代码生成上下文判断，而不是符号表中变量的 scopeLevel
        // 如果当前作用域的 scopeLevel == 0，说明在全局作用域，生成 global
        // 如果当前作用域的 scopeLevel > 0，说明在函数内部，生成 alloca
        int currentScopeLevel = symbolTableManager->getCurrentScope()->getScopeLevel();
        bool isGlobal = (currentScopeLevel == 0);
        
        std::vector<int> dimensions;
        bool isArray = !node->dims.empty();
        
        if (varEntry) {
            // 从符号表获取维度信息
            dimensions = varEntry->dimensions;
            isArray = varEntry->isArray;
        }
        
        if (isGlobal)
        {
            // 全局变量
            std::string varName = "@" + node->ident;
            if (varEntry) {
                varEntry->irName = varName;
            }

            if (isArray && !dimensions.empty())
            {
                // 全局数组
                std::string arrayType = getLLVMArrayType(DataType::INT, dimensions);
                irStream << varName << " = global " << arrayType << " zeroinitializer\n";
            }
            else
            {
                // 全局标量
                int initValue = 0;
                if (node->initVal && node->initVal->isScalar && node->initVal->scalarVal)
                {
                    // 全局变量的初始值必须是常量表达式
                    // ExpNode 实际上就是 AddExpNode（因为 exp: addExp）
                    AddExpNode* addExp = dynamic_cast<AddExpNode*>(node->initVal->scalarVal.get());
                    if (addExp)
                    {
                        initValue = evaluateAddExp(addExp);
                    }
                    else
                    {
                        std::cerr << "Warning: Global variable '" << node->ident 
                                  << "' has invalid initializer, using 0" << std::endl;
                        initValue = 0;
                    }
                }
                irStream << varName << " = global i32 " << initValue << "\n";
            }
        }
        else
        {
            // 局部变量 - 生成 alloca 指令
            // 根本性修复：使用唯一的IR名称，避免不同作用域的变量重名
            // 在LLVM IR中，每个局部变量必须有唯一的名称
            std::string varName = "%" + node->ident;
            
            // 为了避免重名（例如不同if分支中的同名变量），我们需要检查并生成唯一名称
            // 解决方案：使用全局计数器为每个变量名生成唯一后缀
            if (varEntry) {
                // 使用静态map记录每个变量名在当前函数中出现的次数
                // 使用作用域指针的地址作为唯一标识
                static std::map<void*, std::map<std::string, int>> funcVarCounters;
                void* funcScope = symbolTableManager->getCurrentScope();
                
                auto& varCounter = funcVarCounters[funcScope];
                
                if (varCounter.find(node->ident) != varCounter.end()) {
                    // 已经存在同名变量，使用计数器生成唯一名称
                    int count = varCounter[node->ident];
                    varCounter[node->ident]++;
                    varName = "%" + node->ident + std::to_string(count);
                } else {
                    // 第一次出现，记录但不添加后缀
                    varCounter[node->ident] = 1;
                    varName = "%" + node->ident;
                }
                
                varEntry->irName = varName;
            }

            if (isArray && !dimensions.empty())
            {
                // 局部数组
                std::string arrayType = getLLVMArrayType(DataType::INT, dimensions);
                irStream << "  " << varName << " = alloca " << arrayType << "\n";
                
                // 修复：处理数组初始化
                if (node->initVal)
                {
                    std::vector<int> indices;
                    initializeArrayRecursive(varName, dimensions, node->initVal.get(), 0, indices);
                }
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
            // 数组初始化 - 这个函数现在主要用于表达式求值
            // 实际的数组初始化在 visitVarDef 中通过 initializeArrayRecursive 处理
            for (auto &val : node->arrayVals)
            {
                val->accept(this);
            }
        }
    }
    
    // 数组初始化辅助函数：递归处理多维数组初始化
    void CodeGenerator::initializeArrayRecursive(const std::string& arrayName, 
                                                  const std::vector<int>& dimensions,
                                                  InitValNode* initVal, 
                                                  int currentDim,
                                                  std::vector<int>& indices)
    {
        if (!initVal || currentDim >= static_cast<int>(dimensions.size())) return;
        
        if (initVal->isScalar)
        {
            // 标量值：扁平化初始化（如 {1, 2, 3, 4} 用于二维数组）
            // 需要计算当前标量值应该存储到哪个位置
            int flatIndex = 0;
            int multiplier = 1;
            for (int i = static_cast<int>(indices.size()); i < static_cast<int>(dimensions.size()); ++i)
            {
                multiplier *= dimensions[i];
            }
            for (size_t i = 0; i < indices.size(); ++i)
            {
                int localMultiplier = 1;
                for (size_t j = i + 1; j < dimensions.size(); ++j)
                {
                    localMultiplier *= dimensions[j];
                }
                flatIndex += indices[i] * localMultiplier;
            }
            
            // 补齐索引到完整维度
            std::vector<int> fullIndices = indices;
            while (static_cast<int>(fullIndices.size()) < static_cast<int>(dimensions.size()))
            {
                int remainingFlat = flatIndex;
                for (size_t i = fullIndices.size() + 1; i < dimensions.size(); ++i)
                {
                    remainingFlat /= dimensions[i];
                }
                fullIndices.push_back(remainingFlat % dimensions[fullIndices.size()]);
            }
            
            // 计算元素地址
            std::string ptrReg = "%t" + std::to_string(tempCounter++);
            irStream << "  " << ptrReg << " = getelementptr ";
            irStream << getLLVMArrayType(DataType::INT, dimensions);
            irStream << ", " << getLLVMArrayType(DataType::INT, dimensions) << "* ";
            irStream << arrayName;
            irStream << ", i32 0";
            for (int idx : fullIndices)
            {
                irStream << ", i32 " << idx;
            }
            irStream << "\n";
            
            // 求值并存储
            initVal->scalarVal->accept(this);
            irStream << "  store i32 " << currentValue << ", i32* " << ptrReg << "\n";
        }
        else
        {
            // 数组值：递归处理
            int dimSize = dimensions[currentDim];
            int providedValues = static_cast<int>(initVal->arrayVals.size());
            
            for (int i = 0; i < dimSize && i < providedValues; ++i)
            {
                indices.push_back(i);
                
                if (currentDim + 1 < static_cast<int>(dimensions.size()))
                {
                    // 还有更多维度，继续递归
                    if (initVal->arrayVals[i]->isScalar)
                    {
                        // 扁平化初始化：标量值直接存储
                        initializeArrayRecursive(arrayName, dimensions, 
                                                initVal->arrayVals[i].get(), 
                                                currentDim + 1, indices);
                    }
                    else
                    {
                        // 嵌套数组初始化
                        initializeArrayRecursive(arrayName, dimensions, 
                                                initVal->arrayVals[i].get(), 
                                                currentDim + 1, indices);
                    }
                }
                else
                {
                    // 最后一维，处理标量值
                    if (initVal->arrayVals[i]->isScalar && initVal->arrayVals[i]->scalarVal)
                    {
                        // 计算元素地址
                        std::string ptrReg = "%t" + std::to_string(tempCounter++);
                        irStream << "  " << ptrReg << " = getelementptr ";
                        irStream << getLLVMArrayType(DataType::INT, dimensions);
                        irStream << ", " << getLLVMArrayType(DataType::INT, dimensions) << "* ";
                        irStream << arrayName;
                        irStream << ", i32 0";
                        for (int idx : indices)
                        {
                            irStream << ", i32 " << idx;
                        }
                        irStream << "\n";
                        
                        // 求值并存储
                        initVal->arrayVals[i]->scalarVal->accept(this);
                        irStream << "  store i32 " << currentValue << ", i32* " << ptrReg << "\n";
                    }
                }
                
                indices.pop_back();
            }
        }
    }

    void CodeGenerator::visitFuncDef(FuncDefNode *node)
    {
        // 查找函数符号表条目
        FunctionEntry *funcEntry = symbolTableManager->lookupFunction(node->ident);
        if (!funcEntry) {
            std::cerr << "Error: Function '" << node->ident << "' not found in symbol table" << std::endl;
            return;
        }

        // 关键修复：进入函数作用域，这样函数内部的变量会生成 alloca 而不是 global
        symbolTableManager->enterFunction(funcEntry);

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
        
        // 关键修复：退出函数作用域
        symbolTableManager->exitFunction();
    }

    void CodeGenerator::visitFuncFParam(FuncFParamNode *node)
    {
        // 查找符号表条目
        SymbolEntry *paramEntry = symbolTableManager->lookup(node->ident);
        if (!paramEntry) {
            std::cerr << "Error: Parameter '" << node->ident << "' not found in symbol table" << std::endl;
            return;
        }

        VariableEntry *varEntry = dynamic_cast<VariableEntry *>(paramEntry);
        if (!varEntry) {
            std::cerr << "Error: '" << node->ident << "' is not a variable" << std::endl;
            return;
        }

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
            if (!entry) {
                std::cerr << "Error: Variable '" << node->lVal->ident << "' not found in symbol table" << std::endl;
                return;
            }

            VariableEntry *varEntry = dynamic_cast<VariableEntry *>(entry);
            if (!varEntry) {
                std::cerr << "Error: '" << node->lVal->ident << "' is not a variable" << std::endl;
                return;
            }
            
            // 检查变量是否已经生成了IR名称（即是否已经定义了）
            if (varEntry->irName.empty()) {
                std::cerr << "Error: Variable '" << node->lVal->ident << "' has not been defined (no IR name)" << std::endl;
                return;
            }

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
                        // 检查是否为数组参数
                        ParameterEntry *paramEntry = dynamic_cast<ParameterEntry *>(varEntry);
                        bool isArrayParam = (paramEntry != nullptr && paramEntry->isArrayParam);
                        
                        std::string basePtr = varEntry->irName;
                        
                        if (isArrayParam)
                        {
                            // 数组参数：需要先 load i32* 从 i32** 中
                            std::string loadedPtr = "%t" + std::to_string(tempCounter++);
                            irStream << "  " << loadedPtr << " = load i32*, i32** " << basePtr << "\n";
                            basePtr = loadedPtr;
                            
                            // 数组参数：使用 i32* 类型，直接索引
                            irStream << "  " << ptrReg << " = getelementptr i32, i32* " << basePtr;
                            for (const auto &idx : indexValues)
                            {
                                irStream << ", i32 " << idx;
                            }
                            irStream << "\n";
                        }
                        else
                        {
                            // 局部数组：使用数组类型
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
            // 确保 currentValue 不为空
            if (currentValue.empty()) {
                // 如果表达式访问没有设置 currentValue，使用默认值 0
                // 这通常不应该发生，但作为安全措施
                currentValue = "0";
            }
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
        else if (node->primaryExp)
        {
            // 没有一元运算符，直接访问 PrimaryExp
            node->primaryExp->accept(this);
        }
        else if (node->funcCall)
        {
            // 函数调用
            node->funcCall->accept(this);
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
        if (!entry) {
            std::cerr << "Error: Variable '" << node->ident << "' not found in symbol table" << std::endl;
            // 设置一个默认值，避免后续代码崩溃
            currentValue = "0";
            return;
        }

        VariableEntry *varEntry = dynamic_cast<VariableEntry *>(entry);
        if (!varEntry) {
            std::cerr << "Error: '" << node->ident << "' is not a variable" << std::endl;
            // 设置一个默认值，避免后续代码崩溃
            currentValue = "0";
            return;
        }
        
        // 检查变量是否已经生成了IR名称（即是否已经定义了）
        if (varEntry->irName.empty()) {
            std::cerr << "Error: Variable '" << node->ident << "' has not been defined (no IR name)" << std::endl;
            // 设置一个默认值，避免后续代码崩溃
            currentValue = "0";
            return;
        }

        if (node->indices.empty())
        {
            if (varEntry->isArray)
            {
                // 根本性修复：区分数组参数和局部数组
                ParameterEntry *paramEntry = dynamic_cast<ParameterEntry *>(varEntry);
                bool isArrayParam = (paramEntry != nullptr && paramEntry->isArrayParam);
                
                if (isArrayParam)
                {
                    // 数组参数：直接从 i32** load i32*
                    // 当数组参数作为实参传递给另一个函数时，需要这样处理
                    std::string resultReg = "%t" + std::to_string(tempCounter++);
                    irStream << "  " << resultReg << " = load i32*, i32** " << varEntry->irName << "\n";
                    currentValue = resultReg;
                }
                else
                {
                    // 局部数组或全局数组：返回指向第一个元素的指针
                    std::string resultReg = "%t" + std::to_string(tempCounter++);
                    if (varEntry->getScopeLevel() == 0)
                    {
                        // 全局数组
                        irStream << "  " << resultReg << " = getelementptr ";
                        irStream << getLLVMArrayType(DataType::INT, varEntry->dimensions);
                        irStream << ", " << getLLVMArrayType(DataType::INT, varEntry->dimensions) << "* ";
                        irStream << varEntry->irName << ", i32 0, i32 0\n";
                    }
                    else
                    {
                        // 局部数组
                        irStream << "  " << resultReg << " = getelementptr ";
                        irStream << getLLVMArrayType(DataType::INT, varEntry->dimensions);
                        irStream << ", " << getLLVMArrayType(DataType::INT, varEntry->dimensions) << "* ";
                        irStream << varEntry->irName << ", i32 0, i32 0\n";
                    }
                    currentValue = resultReg;
                }
            }
            else
            {
                // 标量变量，生成load指令
                std::string resultReg = "%t" + std::to_string(tempCounter++);
                irStream << "  " << resultReg << " = load i32, i32* " << varEntry->irName << "\n";
                currentValue = resultReg;
            }
        }
        else
        {
            // 数组元素访问或部分索引（多维数组）
            // 计算所有索引表达式
            std::vector<std::string> indexValues;
            for (auto &index : node->indices)
            {
                index->accept(this);
                indexValues.push_back(currentValue);
            }

            // 根本性修复：判断是否为部分索引（多维数组）
            // 如果索引数量少于数组维度，则返回子数组指针，不进行load
            bool isPartialIndex = (indexValues.size() < varEntry->dimensions.size());

            // 生成 getelementptr 指令获取数组元素地址或子数组指针
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
                
                // 根本性修复：对于部分索引，添加额外的 ", i32 0" 来获取 i32* 而不是子数组指针
                // 这样 c[0] 会返回 i32* 而不是 [4 x i32]*
                if (isPartialIndex)
                {
                    irStream << ", i32 0";
                }
                
                irStream << "\n";
            }
            else
            {
                // 局部数组或数组参数
                // 检查是否为数组参数
                ParameterEntry *paramEntry = dynamic_cast<ParameterEntry *>(varEntry);
                bool isArrayParam = (paramEntry != nullptr && paramEntry->isArrayParam);
                
                std::string basePtr = varEntry->irName;
                
                if (isArrayParam)
                {
                    // 数组参数：需要先 load i32* 从 i32** 中
                    std::string loadedPtr = "%t" + std::to_string(tempCounter++);
                    irStream << "  " << loadedPtr << " = load i32*, i32** " << basePtr << "\n";
                    basePtr = loadedPtr;
                    
                    // 数组参数：使用 i32* 类型，直接索引
                    irStream << "  " << ptrReg << " = getelementptr i32, i32* " << basePtr;
                    for (const auto &idx : indexValues)
                    {
                        irStream << ", i32 " << idx;
                    }
                    irStream << "\n";
                }
                else
                {
                    // 局部数组：使用数组类型
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
                    
                    // 根本性修复：对于部分索引，添加额外的 ", i32 0" 来获取 i32* 而不是子数组指针
                    if (isPartialIndex)
                    {
                        irStream << ", i32 0";
                    }
                    
                    irStream << "\n";
                }
            }

            // 根本性修复：只有在完整索引时才load值
            // 如果是部分索引（多维数组），返回指针，不load
            if (isPartialIndex)
            {
                // 部分索引：返回 i32* 指针（指向子数组的第一个元素）
                // 例如：c[0] 其中 c 是 [1024][4]，返回 i32*
                currentValue = ptrReg;
            }
            else
            {
                // 完整索引：从数组元素地址加载值
                std::string resultReg = "%t" + std::to_string(tempCounter++);
                irStream << "  " << resultReg << " = load i32, i32* " << ptrReg << "\n";
                currentValue = resultReg;
            }
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
        DataType returnType = DataType::VOID;
        bool isSystemFunc = false;
        
        if (!funcEntry) {
            // 检查是否为系统函数
            if (isSystemFunction(node->ident)) {
                isSystemFunc = true;
                returnType = getSystemFunctionReturnType(node->ident);
            } else {
                std::cerr << "Error: Function '" << node->ident << "' not found in symbol table" << std::endl;
                // 设置一个默认返回值，避免后续代码崩溃
                currentValue = "0";
                return;
            }
        } else {
            returnType = funcEntry->getReturnType();
        }

        // 计算所有参数，并确定参数类型
        std::vector<std::string> argValues;
        std::vector<std::string> argTypes;
        
        for (size_t i = 0; i < node->args.size(); ++i)
        {
            node->args[i]->accept(this);
            argValues.push_back(currentValue);
            
            // 确定参数类型
            std::string paramType = "i32";  // 默认类型
            if (funcEntry && i < static_cast<size_t>(funcEntry->getParameterCount()))
            {
                VariableEntry *param = funcEntry->getParameter(static_cast<int>(i));
                if (param) {
                    ParameterEntry *paramEntry = dynamic_cast<ParameterEntry *>(param);
                    if (paramEntry && paramEntry->isArrayParam)
                    {
                        paramType = "i32*";  // 数组参数
                    }
                }
            }
            argTypes.push_back(paramType);
        }

        // 生成调用指令
        std::string funcName = "@" + node->ident;

        if (returnType == DataType::VOID)
        {
            irStream << "  call void " << funcName << "(";
            for (size_t i = 0; i < argValues.size(); ++i)
            {
                if (i > 0)
                    irStream << ", ";
                irStream << argTypes[i] << " " << argValues[i];
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
                irStream << argTypes[i] << " " << argValues[i];
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

    // 常量表达式求值函数（用于全局变量初始值）
    int CodeGenerator::evaluateConstExp(ConstExpNode* node) {
        if (!node || !node->addExp) return 0;
        return evaluateAddExp(node->addExp.get());
    }
    
    int CodeGenerator::evaluateAddExp(AddExpNode* node) {
        if (!node) return 0;
        
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
    
    int CodeGenerator::evaluateMulExp(MulExpNode* node) {
        if (!node) return 0;
        
        if (!node->left) {
            return evaluateUnaryExp(node->right.get());
        }
        
        int leftVal = evaluateMulExp(node->left.get());
        int rightVal = evaluateUnaryExp(node->right.get());
        
        if (node->op == BinaryOp::MUL) {
            return leftVal * rightVal;
        } else if (node->op == BinaryOp::DIV) {
            if (rightVal == 0) {
                std::cerr << "Warning: Division by zero in constant expression" << std::endl;
                return 0;
            }
            return leftVal / rightVal;
        } else {
            if (rightVal == 0) {
                std::cerr << "Warning: Modulo by zero in constant expression" << std::endl;
                return 0;
            }
            return leftVal % rightVal;
        }
    }
    
    int CodeGenerator::evaluateUnaryExp(UnaryExpNode* node) {
        if (!node) return 0;
        
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
        
        if (node->primaryExp) {
            return evaluatePrimaryExp(node->primaryExp.get());
        }
        
        if (node->funcCall) {
            std::cerr << "Warning: Function call in constant expression" << std::endl;
            return 0;
        }
        
        return 0;
    }
    
    int CodeGenerator::evaluatePrimaryExp(PrimaryExpNode* node) {
        if (!node) return 0;
        
        if (node->type == PrimaryExpType::NUMBER) {
            return node->number ? node->number->value : 0;
        } else if (node->type == PrimaryExpType::LVAL) {
            return evaluateLVal(node->lVal.get());
        } else if (node->type == PrimaryExpType::PAREN_EXP && node->exp) {
            AddExpNode* addExp = dynamic_cast<AddExpNode*>(node->exp.get());
            if (addExp) {
                return evaluateAddExp(addExp);
            }
        }
        
        return 0;
    }
    
    int CodeGenerator::evaluateLVal(LValNode* node) {
        if (!node) return 0;
        
        // 查找常量变量
        VariableEntry* var = symbolTableManager->lookupVariable(node->ident);
        if (!var || !var->isConst) {
            std::cerr << "Warning: Non-constant variable in constant expression: " << node->ident << std::endl;
            return 0;
        }
        
        if (!node->indices.empty()) {
            std::cerr << "Warning: Array element access in constant expression not fully supported" << std::endl;
            return 0;
        }
        
        if (var->hasConstValue && !var->isArray) {
            return var->constValue;
        }
        
        return 0;
    }

    // 生成系统函数声明
    void CodeGenerator::generateSystemFunctionDeclarations()
    {
        // 根据 sylib.h 中的定义生成系统函数声明
        irStream << "; System function declarations\n";
        irStream << "declare i32 @getint()\n";
        irStream << "declare i32 @getch()\n";
        irStream << "declare i32 @getarray(i32*)\n";
        irStream << "declare void @putint(i32)\n";
        irStream << "declare void @putch(i32)\n";
        irStream << "declare void @putarray(i32, i32*)\n";
        irStream << "\n";
    }

    // 检查是否为系统函数
    bool CodeGenerator::isSystemFunction(const std::string &funcName)
    {
        return funcName == "getint" || funcName == "getch" || funcName == "getarray" ||
               funcName == "putint" || funcName == "putch" || funcName == "putarray" ||
               funcName == "getfloat" || funcName == "getfarray" ||
               funcName == "putfloat" || funcName == "putfarray" || funcName == "putf" ||
               funcName == "_sysy_starttime" || funcName == "_sysy_stoptime" ||
               funcName == "before_main" || funcName == "after_main";
    }

    // 获取系统函数的返回类型
    DataType CodeGenerator::getSystemFunctionReturnType(const std::string &funcName)
    {
        if (funcName == "getint" || funcName == "getch" || funcName == "getarray" ||
            funcName == "getfloat" || funcName == "getfarray")
        {
            return DataType::INT; // 注意：getfloat 实际返回 float，但当前只支持 int
        }
        return DataType::VOID;
    }

    // 获取系统函数的参数数量
    int CodeGenerator::getSystemFunctionParamCount(const std::string &funcName)
    {
        if (funcName == "getint" || funcName == "getch")
        {
            return 0;
        }
        else if (funcName == "putint" || funcName == "putch")
        {
            return 1;
        }
        else if (funcName == "getarray" || funcName == "putfloat")
        {
            return 1;
        }
        else if (funcName == "putarray" || funcName == "getfarray" || funcName == "putfarray")
        {
            return 2;
        }
        else if (funcName == "putf")
        {
            return -1; // 可变参数
        }
        else if (funcName == "_sysy_starttime" || funcName == "_sysy_stoptime")
        {
            return 1;
        }
        else if (funcName == "before_main" || funcName == "after_main")
        {
            return 0;
        }
        return 0;
    }

} // namespace sysy
