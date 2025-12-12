#include "CodeGenerator.h"
#include "Nodes.h"
#include <sstream>
#include <map>
#include <iostream>
#include <algorithm>

namespace sysy
{
    // 用于存储表达式结果的临时变量计数器
    static int tempCounter = 0;

    // 用于存储当前表达式的结果
    static std::string currentValue;

    // 用于存储条件表达式的结果寄存器
    static std::string condResultReg;

    CodeGenerator::CodeGenerator(SymbolTableManager *symTabMgr)
        : symbolTableManager(symTabMgr), labelCounter(0), currentParamIndex(0), currentBlockLabel(""), inFunctionFirstBlock(false), collectingAllocas(false), currentAllocaIndex(0)
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
                std::string arrayType = getLLVMArrayType(DataType::INT, dimensions);
                int total = getTotalElements(dimensions);
                std::vector<int> values(total, 0);
                int linearIndex = 0;
                if (node->initVal)
                {
                    fillConstInitVector(dimensions, 0, node->initVal.get(), linearIndex, values);
                }
                std::string constAgg = buildArrayConstant(dimensions, values);
                irStream << constName << " = constant " << arrayType << " " << constAgg << "\n";
            }
            else
            {
                // 全局常量标量
                int initValue = 0;
                if (node->initVal && node->initVal->isScalar && node->initVal->scalarVal)
                {
                    // 计算常量初始值（纯常量表达式）
                    initValue = evaluateConstExp(node->initVal->scalarVal.get());
                }
                irStream << constName << " = constant i32 " << initValue << "\n";
            }
        }
        else
        {
            // 局部常量 - alloca已在函数入口块生成（alloca hoisting）
            // 这里只处理初始化
            
            // 从pendingAllocas获取已分配的IR名称
            std::string constName = getNextAllocaIrName(node->ident);
            
            // 更新符号表条目的irName
            if (constEntry) {
                constEntry->irName = constName;
            }

            if (isArray && !dimensions.empty())
            {
                // 局部常量数组 - 处理初始化
                if (node->initVal)
                {
                    initializeConstLocalArray(constName, dimensions, node->initVal.get());
                }
            }
            else
            {
                // 局部常量标量 - 如果有初始值，生成 store 指令
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
                if (node->initVal)
                {
                    // 有初始化器时，生成完整常量聚合
                    int total = getTotalElements(dimensions);
                    std::vector<int> values(total, 0);
                    int linearIndex = 0;
                    fillInitVector(dimensions, 0, node->initVal.get(), linearIndex, values);
                    std::string constAgg = buildArrayConstant(dimensions, values);
                    irStream << varName << " = global " << arrayType << " " << constAgg << "\n";
                }
                else
                {
                    // 无初始化器时，使用 zeroinitializer
                    irStream << varName << " = global " << arrayType << " zeroinitializer\n";
                }
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
            // 局部变量 - alloca已在函数入口块生成（alloca hoisting）
            // 这里只处理初始化
            
            // 从pendingAllocas获取已分配的IR名称
            std::string varName = getNextAllocaIrName(node->ident);
            
            // 更新符号表条目的irName
            if (varEntry) {
                varEntry->irName = varName;
            }

            if (isArray && !dimensions.empty())
            {
                // 局部数组 - 处理初始化
                if (node->initVal)
                {
                    initializeLocalArray(varName, dimensions, node->initVal.get());
                }
            }
            else
            {
                // 局部标量 - 如果有初始值，生成 store 指令
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
            // 实际的数组初始化在 visitVarDef 中通过 initializeLocalArray 处理
            for (auto &val : node->arrayVals)
            {
                val->accept(this);
            }
        }
    }
    
    int CodeGenerator::getTotalElements(const std::vector<int> &dimensions)
    {
        int total = 1;
        for (int dim : dimensions)
        {
            total *= dim;
        }
        return total;
    }

    std::vector<int> CodeGenerator::linearToIndices(int linearIndex, const std::vector<int> &dimensions)
    {
        std::vector<int> indices(dimensions.size(), 0);
        int remainder = linearIndex;
        for (size_t i = 0; i < dimensions.size(); ++i)
        {
            int stride = 1;
            for (size_t j = i + 1; j < dimensions.size(); ++j)
            {
                stride *= dimensions[j];
            }
            if (stride != 0)
            {
                indices[i] = remainder / stride;
                remainder %= stride;
            }
        }
        return indices;
    }

    std::string CodeGenerator::emitArrayElementPtr(const std::string &arrayName, const std::vector<int> &dimensions,
                                                   const std::vector<int> &indices)
    {
        std::string ptrReg = "%t" + std::to_string(tempCounter++);
        irStream << "  " << ptrReg << " = getelementptr ";
        irStream << getLLVMArrayType(DataType::INT, dimensions);
        irStream << ", " << getLLVMArrayType(DataType::INT, dimensions) << "* " << arrayName;
        irStream << ", i32 0";
        for (int idx : indices)
        {
            irStream << ", i32 " << idx;
        }
        irStream << "\n";
        return ptrReg;
    }

    void CodeGenerator::zeroInitializeArray(const std::string &arrayName, const std::vector<int> &dimensions)
    {
        int total = getTotalElements(dimensions);
        // 对于大数组，使用 memset 优化；对于小数组，逐个 store
        const int MEMSET_THRESHOLD = 64;
        if (total > MEMSET_THRESHOLD)
        {
            // 使用 llvm.memset 进行零初始化
            std::string arrayType = getLLVMArrayType(DataType::INT, dimensions);
            std::string ptrReg = "%memset_ptr" + std::to_string(tempCounter++);
            irStream << "  " << ptrReg << " = bitcast " << arrayType << "* " << arrayName << " to i8*\n";
            int byteSize = total * 4; // i32 = 4 bytes
            irStream << "  call void @llvm.memset.p0i8.i64(i8* " << ptrReg << ", i8 0, i64 " << byteSize << ", i1 false)\n";
        }
        else
        {
            for (int i = 0; i < total; ++i)
            {
                std::vector<int> indices = linearToIndices(i, dimensions);
                std::string ptrReg = emitArrayElementPtr(arrayName, dimensions, indices);
                irStream << "  store i32 0, i32* " << ptrReg << "\n";
            }
        }
    }

    void CodeGenerator::fillArrayFromInit(const std::string &arrayName, const std::vector<int> &dimensions,
                                          int dimIndex, InitValNode *initVal, int &linearIndex)
    {
        if (!initVal)
            return;

        int total = getTotalElements(dimensions);
        if (linearIndex >= total)
            return;

        // 标量：直接写入当前位置
        if (initVal->isScalar)
        {
            if (!initVal->scalarVal)
                return;
            std::vector<int> indices = linearToIndices(linearIndex, dimensions);
            std::string ptrReg = emitArrayElementPtr(arrayName, dimensions, indices);
            initVal->scalarVal->accept(this);
            irStream << "  store i32 " << currentValue << ", i32* " << ptrReg << "\n";
            ++linearIndex;
            return;
        }

        // 计算当前维度一个元素所占的标量数量，用于对齐到子聚合边界
        int stride = 1;
        for (size_t i = dimIndex + 1; i < dimensions.size(); ++i)
        {
            stride *= dimensions[i];
        }

        for (auto &childPtr : initVal->arrayVals)
        {
            if (linearIndex >= total)
                break;

            InitValNode *child = childPtr.get();
            if (!child)
                continue;

            if (child->isScalar && child->scalarVal)
            {
                std::vector<int> indices = linearToIndices(linearIndex, dimensions);
                std::string ptrReg = emitArrayElementPtr(arrayName, dimensions, indices);
                child->scalarVal->accept(this);
                irStream << "  store i32 " << currentValue << ", i32* " << ptrReg << "\n";
                ++linearIndex;
            }
            else
            {
                int base = ((linearIndex + stride - 1) / stride) * stride;
                linearIndex = base;
                fillArrayFromInit(arrayName, dimensions, dimIndex + 1, child, linearIndex);
                linearIndex = base + stride;
            }
        }
    }

    void CodeGenerator::fillConstArrayFromInit(const std::string &arrayName, const std::vector<int> &dimensions,
                                               int dimIndex, ConstInitValNode *initVal, int &linearIndex)
    {
        if (!initVal)
            return;

        int total = getTotalElements(dimensions);
        if (linearIndex >= total)
            return;

        if (initVal->isScalar)
        {
            if (!initVal->scalarVal)
                return;
            std::vector<int> indices = linearToIndices(linearIndex, dimensions);
            std::string ptrReg = emitArrayElementPtr(arrayName, dimensions, indices);
            initVal->scalarVal->accept(this);
            irStream << "  store i32 " << currentValue << ", i32* " << ptrReg << "\n";
            ++linearIndex;
            return;
        }

        int stride = 1;
        for (size_t i = dimIndex + 1; i < dimensions.size(); ++i)
        {
            stride *= dimensions[i];
        }

        for (auto &childPtr : initVal->arrayVals)
        {
            if (linearIndex >= total)
                break;

            ConstInitValNode *child = childPtr.get();
            if (!child)
                continue;

            if (child->isScalar && child->scalarVal)
            {
                std::vector<int> indices = linearToIndices(linearIndex, dimensions);
                std::string ptrReg = emitArrayElementPtr(arrayName, dimensions, indices);
                child->scalarVal->accept(this);
                irStream << "  store i32 " << currentValue << ", i32* " << ptrReg << "\n";
                ++linearIndex;
            }
            else
            {
                int base = ((linearIndex + stride - 1) / stride) * stride;
                linearIndex = base;
                fillConstArrayFromInit(arrayName, dimensions, dimIndex + 1, child, linearIndex);
                linearIndex = base + stride;
            }
        }
    }

    void CodeGenerator::initializeConstLocalArray(const std::string &arrayName, const std::vector<int> &dimensions, ConstInitValNode *initVal)
    {
        if (!initVal)
            return;

        zeroInitializeArray(arrayName, dimensions);

        int linearIndex = 0;
        fillConstArrayFromInit(arrayName, dimensions, 0, initVal, linearIndex);
    }

    void CodeGenerator::fillConstInitVector(const std::vector<int> &dimensions, int dimIndex,
                                            ConstInitValNode *initVal, int &linearIndex, std::vector<int> &values)
    {
        if (!initVal)
            return;

        int total = getTotalElements(dimensions);
        if (linearIndex >= total)
            return;

        if (initVal->isScalar)
        {
            if (!initVal->scalarVal)
                return;
            values[linearIndex++] = evaluateConstExp(initVal->scalarVal.get());
            return;
        }

        int stride = 1;
        for (size_t i = dimIndex + 1; i < dimensions.size(); ++i)
        {
            stride *= dimensions[i];
        }

        for (auto &childPtr : initVal->arrayVals)
        {
            if (linearIndex >= total)
                break;

            ConstInitValNode *child = childPtr.get();
            if (!child)
                continue;

            if (child->isScalar && child->scalarVal)
            {
                values[linearIndex++] = evaluateConstExp(child->scalarVal.get());
            }
            else
            {
                int base = ((linearIndex + stride - 1) / stride) * stride;
                linearIndex = base;
                fillConstInitVector(dimensions, dimIndex + 1, child, linearIndex, values);
                linearIndex = base + stride;
            }
        }
    }

    void CodeGenerator::fillInitVector(const std::vector<int> &dimensions, int dimIndex,
                                       InitValNode *initVal, int &linearIndex, std::vector<int> &values)
    {
        if (!initVal)
            return;

        int total = getTotalElements(dimensions);
        if (linearIndex >= total)
            return;

        if (initVal->isScalar)
        {
            if (!initVal->scalarVal)
                return;
            AddExpNode *addExp = dynamic_cast<AddExpNode *>(initVal->scalarVal.get());
            int val = 0;
            if (addExp)
            {
                val = evaluateAddExp(addExp);
            }
            values[linearIndex++] = val;
            return;
        }

        int stride = 1;
        for (size_t i = dimIndex + 1; i < dimensions.size(); ++i)
        {
            stride *= dimensions[i];
        }

        for (auto &childPtr : initVal->arrayVals)
        {
            if (linearIndex >= total)
                break;

            InitValNode *child = childPtr.get();
            if (!child)
                continue;

            if (child->isScalar && child->scalarVal)
            {
                AddExpNode *addExp = dynamic_cast<AddExpNode *>(child->scalarVal.get());
                int val = 0;
                if (addExp)
                {
                    val = evaluateAddExp(addExp);
                }
                values[linearIndex++] = val;
            }
            else
            {
                int base = ((linearIndex + stride - 1) / stride) * stride;
                linearIndex = base;
                fillInitVector(dimensions, dimIndex + 1, child, linearIndex, values);
                linearIndex = base + stride;
            }
        }
    }

    std::string CodeGenerator::buildArrayConstantRecursive(const std::vector<int> &dimensions, const std::vector<int> &values,
                                                           int dimIndex, int &linearIndex)
    {
        bool isLast = (dimIndex == static_cast<int>(dimensions.size()) - 1);
        int count = dimensions[dimIndex];
        std::ostringstream oss;
        oss << "[";

        for (int i = 0; i < count; ++i)
        {
            if (i > 0)
                oss << ", ";

            if (isLast)
            {
                oss << "i32 " << values[linearIndex++];
            }
            else
            {
                std::vector<int> subDims(dimensions.begin() + dimIndex + 1, dimensions.end());
                std::string elemType = getLLVMArrayType(DataType::INT, subDims);
                std::string sub = buildArrayConstantRecursive(dimensions, values, dimIndex + 1, linearIndex);
                oss << elemType << " " << sub;
            }
        }

        oss << "]";
        return oss.str();
    }

    std::string CodeGenerator::buildArrayConstant(const std::vector<int> &dimensions, const std::vector<int> &values)
    {
        int idx = 0;
        return buildArrayConstantRecursive(dimensions, values, 0, idx);
    }

    // 计算多维数组参数的线性索引
    // 对于 int a[][d0][d1]...，索引 [i0][i1][i2]... 的线性索引计算方式：
    // linearIndex = i0 * (d0 * d1 * ...) + i1 * (d1 * ...) + i2 * ...
    // 需要生成 mul/add IR 指令序列来动态计算
    std::string CodeGenerator::computeLinearIndex(const std::vector<std::string>& indexValues, const std::vector<int>& arrayDims)
    {
        // 如果只有一个索引，直接返回
        if (indexValues.size() == 1) {
            return indexValues[0];
        }
        
        // 外部计数器变量已定义在文件开头
        extern int tempCounter;
        
        // 首先计算每个维度的 stride（步长）
        // 对于 int a[][5]，stride[0] = 5, stride[1] = 1
        // 对于 int a[][3][4]，stride[0] = 3*4 = 12, stride[1] = 4, stride[2] = 1
        std::vector<int> strides(indexValues.size(), 1);
        for (int i = static_cast<int>(indexValues.size()) - 2; i >= 0; --i) {
            // strides[i] = strides[i+1] * arrayDims[i]
            // arrayDims[i] 是第 i+1 个维度的大小（因为第一维是缺失的）
            if (static_cast<size_t>(i) < arrayDims.size()) {
                strides[i] = strides[i + 1] * arrayDims[i];
            } else {
                strides[i] = strides[i + 1];
            }
        }
        
        // 生成 linearIndex = sum(indexValues[i] * strides[i])
        std::string result = "";
        
        for (size_t i = 0; i < indexValues.size(); ++i) {
            std::string term;
            if (strides[i] == 1) {
                term = indexValues[i];
            } else {
                // term = indexValues[i] * strides[i]
                std::string mulReg = "%t" + std::to_string(tempCounter++);
                irStream << "  " << mulReg << " = mul i32 " << indexValues[i] << ", " << strides[i] << "\n";
                term = mulReg;
            }
            
            if (result.empty()) {
                result = term;
            } else {
                // result = result + term
                std::string addReg = "%t" + std::to_string(tempCounter++);
                irStream << "  " << addReg << " = add i32 " << result << ", " << term << "\n";
                result = addReg;
            }
        }
        
        return result;
    }

    void CodeGenerator::initializeLocalArray(const std::string &arrayName, const std::vector<int> &dimensions, InitValNode *initVal)
    {
        if (!initVal)
            return;

        // 先将所有元素置零，保证未提供的值默认为0
        zeroInitializeArray(arrayName, dimensions);

        int linearIndex = 0;
        fillArrayFromInit(arrayName, dimensions, 0, initVal, linearIndex);
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

        // 在任何指令之前生成入口基本块标签
        std::string entryLabel = generateLabel("entry");
        irStream << entryLabel << ":\n";
        currentBlockLabel = entryLabel;

        // 为每个参数调用visitFuncFParam处理
        currentParamIndex = 0;
        for (size_t i = 0; i < node->params.size(); ++i)
        {
            currentParamIndex = i;
            node->params[i]->accept(this);
        }
        
        // ========== Alloca Hoisting: 先收集所有局部变量，在入口块生成alloca ==========
        if (node->block)
        {
            collectLocalVars(node->block.get());
            
            // 在入口块生成所有alloca指令
            for (auto& info : pendingAllocas)
            {
                if (info.entry) {
                    info.entry->irName = info.irName;
                }
                
                if (info.isArray && !info.dimensions.empty())
                {
                    std::string arrayType = getLLVMArrayType(DataType::INT, info.dimensions);
                    irStream << "  " << info.irName << " = alloca " << arrayType << "\n";
                }
                else
                {
                    irStream << "  " << info.irName << " = alloca i32\n";
                }
            }
            
            // 重置alloca索引，用于visitVarDef/visitConstDef匹配
            currentAllocaIndex = 0;
        }

        // 生成函数体
        bool hasReturn = false;
        if (node->block)
        {
            // 设置标志，表示即将处理函数首块
            inFunctionFirstBlock = true;
            
            // 通过 accept 调用 visitBlockStmt，这样内层块也会被正确处理
            node->block->accept(this);
            
            // 检查是否有 return 语句
            for (auto &item : node->block->blockItems)
            {
                if (item->stmt && dynamic_cast<ReturnStmtNode *>(item->stmt.get()))
                {
                    hasReturn = true;
                    break;
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
        // 使用标志判断是否为函数首块
        bool isFirstBlockInFunc = inFunctionFirstBlock;
        inFunctionFirstBlock = false;  // 重置标志，后续块都不是首块
        
        bool enteredScope = false;
        if (!isFirstBlockInFunc) {
            // 进入语义分析时已创建的子作用域（不创建新的）
            enteredScope = symbolTableManager->enterExistingChildScope();
        }
        
        // 遍历所有块项目
        for (auto &item : node->blockItems)
        {
            item->accept(this);
        }
        
        if (enteredScope) {
            symbolTableManager->exitExistingScope();
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
            // 查找变量，需要找到已经有 irName 的那个（已声明的）
            SymbolEntry *entry = nullptr;
            SymbolTable* scope = symbolTableManager->getCurrentScope();
            while (scope != nullptr) {
                SymbolEntry* found = scope->lookup(node->lVal->ident);
                if (found) {
                    VariableEntry* varFound = dynamic_cast<VariableEntry*>(found);
                    if (varFound && !varFound->irName.empty()) {
                        entry = found;
                        break;
                    }
                }
                scope = scope->getParent();
            }
            
            if (!entry) {
                std::cerr << "Error: Variable '" << node->lVal->ident << "' not found in symbol table or has no IR name" << std::endl;
                return;
            }

            VariableEntry *varEntry = dynamic_cast<VariableEntry *>(entry);
            if (!varEntry) {
                std::cerr << "Error: '" << node->lVal->ident << "' is not a variable" << std::endl;
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
                            
                            // 数组参数：使用 i32* 类型，需要计算线性索引
                            // 对于 int a[][5]，访问 a[i][j] 需要计算 i * 5 + j
                            std::string linearIndex = computeLinearIndex(indexValues, paramEntry->arrayDims);
                            irStream << "  " << ptrReg << " = getelementptr i32, i32* " << basePtr;
                            irStream << ", i32 " << linearIndex;
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
        currentBlockLabel = thenLabel;
        if (node->thenStmt)
        {
            node->thenStmt->accept(this);
        }
        irStream << "  br label %" << endLabel << "\n";

        // else分支
        if (node->elseStmt)
        {
            irStream << elseLabel << ":\n";
            currentBlockLabel = elseLabel;
            node->elseStmt->accept(this);
            irStream << "  br label %" << endLabel << "\n";
        }

        // 结束标签
        irStream << endLabel << ":\n";
        currentBlockLabel = endLabel;
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
        currentBlockLabel = condLabel;
        if (node->cond)
        {
            node->cond->accept(this);
            std::string condReg = condResultReg;
            irStream << "  br i1 " << condReg << ", label %" << bodyLabel
                     << ", label %" << endLabel << "\n";
        }

        // 循环体
        irStream << bodyLabel << ":\n";
        currentBlockLabel = bodyLabel;
        if (node->body)
        {
            node->body->accept(this);
        }
        irStream << "  br label %" << condLabel << "\n";

        // 结束标签
        irStream << endLabel << ":\n";
        currentBlockLabel = endLabel;

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
                condResultReg = resultReg; // 供条件使用
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
        // 查找符号表条目 - 需要找到已经有 irName 的那个（已声明的）
        SymbolEntry *entry = nullptr;
        SymbolTable* scope = symbolTableManager->getCurrentScope();
        while (scope != nullptr) {
            SymbolEntry* found = scope->lookup(node->ident);
            if (found) {
                VariableEntry* varFound = dynamic_cast<VariableEntry*>(found);
                if (varFound && !varFound->irName.empty()) {
                    entry = found;
                    break;
                }
            }
            scope = scope->getParent();
        }
        
        if (!entry) {
            std::cerr << "Error: Variable '" << node->ident << "' not found in symbol table or has no IR name" << std::endl;
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
                        // 全局数组：需要足够多的 i32 0 索引来获取 i32*
                        irStream << "  " << resultReg << " = getelementptr ";
                        irStream << getLLVMArrayType(DataType::INT, varEntry->dimensions);
                        irStream << ", " << getLLVMArrayType(DataType::INT, varEntry->dimensions) << "* ";
                        irStream << varEntry->irName;
                        // 第一个 i32 0 是访问数组本身，后续每一个 i32 0 深入一个维度
                        for (size_t d = 0; d <= varEntry->dimensions.size(); ++d)
                        {
                            irStream << ", i32 0";
                        }
                        irStream << "\n";
                    }
                    else
                    {
                        // 局部数组：需要足够多的 i32 0 索引来获取 i32*
                        irStream << "  " << resultReg << " = getelementptr ";
                        irStream << getLLVMArrayType(DataType::INT, varEntry->dimensions);
                        irStream << ", " << getLLVMArrayType(DataType::INT, varEntry->dimensions) << "* ";
                        irStream << varEntry->irName;
                        // 第一个 i32 0 是访问数组本身，后续每一个 i32 0 深入一个维度
                        for (size_t d = 0; d <= varEntry->dimensions.size(); ++d)
                        {
                            irStream << ", i32 0";
                        }
                        irStream << "\n";
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
                
                // 对于部分索引（如多维数组只索引前几维），需要额外添加 i32 0 来获取指向元素的指针
                // 例如：buf[0]（其中 buf 是 [2][100]）应该生成 getelementptr ... @buf, i32 0, i32 0, i32 0
                // 这样才能返回 i32* 而不是 [100 x i32]*
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
                    
                    // 数组参数：使用 i32* 类型，需要计算线性索引
                    // 对于 int a[][5]，访问 a[i][j] 需要计算 i * 5 + j
                    std::string linearIndex = computeLinearIndex(indexValues, paramEntry->arrayDims);
                    irStream << "  " << ptrReg << " = getelementptr i32, i32* " << basePtr;
                    irStream << ", i32 " << linearIndex;
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
                    
                    // 对于部分索引，添加额外的 i32 0 来获取指向元素的指针
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
        
        // 获取系统函数的参数类型（如果是系统函数）
        std::vector<std::string> sysFuncParamTypes;
        if (isSystemFunc)
        {
            sysFuncParamTypes = getSystemFunctionParamTypes(node->ident);
        }
        
        for (size_t i = 0; i < node->args.size(); ++i)
        {
            node->args[i]->accept(this);
            argValues.push_back(currentValue);
            
            // 确定参数类型
            std::string paramType = "i32";  // 默认类型
            if (isSystemFunc && i < sysFuncParamTypes.size())
            {
                // 对于系统函数，使用系统函数定义的参数类型
                paramType = sysFuncParamTypes[i];
            }
            else if (funcEntry && i < static_cast<size_t>(funcEntry->getParameterCount()))
            {
                // 对于用户定义函数，从符号表获取参数类型
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
            // 表达式值为 i32 0/1
            std::string extReg = "%t" + std::to_string(tempCounter++);
            irStream << "  " << extReg << " = zext i1 " << cmpReg << " to i32\n";
            currentValue = extReg;
        }
        else
        {
            // 二元操作 - 短路求值（控制流 + phi）
            node->left->accept(this);
            std::string leftVal = currentValue;

            // 左操作数转为 i1
            std::string leftCmp = "%t" + std::to_string(tempCounter++);
            irStream << "  " << leftCmp << " = icmp ne i32 " << leftVal << ", 0\n";

            // 构造基本块
            std::string rhsLabel = generateLabel("and.rhs");
            std::string endLabel = generateLabel("and.end");
            // 记录左分支所在前驱块标签，用于 phi
            std::string lhsPred = currentBlockLabel;
            // 依据左值进行跳转：false -> end，true -> rhs
            irStream << "  br i1 " << leftCmp << ", label %" << rhsLabel << ", label %" << endLabel << "\n";

            // 右侧块
            irStream << rhsLabel << ":\n";
            currentBlockLabel = rhsLabel;
            node->right->accept(this);
            std::string rightVal = currentValue;
            std::string rightCmp = "%t" + std::to_string(tempCounter++);
            irStream << "  " << rightCmp << " = icmp ne i32 " << rightVal << ", 0\n";
            // 记录右侧实际前驱（可能被嵌套表达式更新）
            std::string rightPred = currentBlockLabel;
            irStream << "  br label %" << endLabel << "\n";

            // 合并块
            irStream << endLabel << ":\n";
            std::string phiReg = "%t" + std::to_string(tempCounter++);
            irStream << "  " << phiReg << " = phi i1 [ false, %" << lhsPred << " ], [ " << rightCmp << ", %" << rightPred << " ]\n";
            currentBlockLabel = endLabel;
            // 设置表达式与条件结果
            std::string extReg = "%t" + std::to_string(tempCounter++);
            irStream << "  " << extReg << " = zext i1 " << phiReg << " to i32\n";
            currentValue = extReg;
            condResultReg = phiReg;
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
            std::string extReg = "%t" + std::to_string(tempCounter++);
            irStream << "  " << extReg << " = zext i1 " << cmpReg << " to i32\n";
            currentValue = extReg;
        }
        else
        {
            // 二元操作 - 短路求值（控制流 + phi）
            node->left->accept(this);
            std::string leftVal = currentValue;

            // 左操作数转为 i1
            std::string leftCmp = "%t" + std::to_string(tempCounter++);
            irStream << "  " << leftCmp << " = icmp ne i32 " << leftVal << ", 0\n";

            // 构造基本块
            std::string rhsLabel = generateLabel("or.rhs");
            std::string endLabel = generateLabel("or.end");
            // 记录左分支所在前驱块标签，用于 phi
            std::string lhsPred = currentBlockLabel;
            // 左值为真则直接到 end，为假则到 rhs
            irStream << "  br i1 " << leftCmp << ", label %" << endLabel << ", label %" << rhsLabel << "\n";

            // 右侧块
            irStream << rhsLabel << ":\n";
            currentBlockLabel = rhsLabel;
            node->right->accept(this);
            std::string rightVal = currentValue;
            std::string rightCmp = "%t" + std::to_string(tempCounter++);
            irStream << "  " << rightCmp << " = icmp ne i32 " << rightVal << ", 0\n";
            // 记录右侧实际前驱（可能被嵌套表达式更新）
            std::string rightPred = currentBlockLabel;
            irStream << "  br label %" << endLabel << "\n";

            // 合并块
            irStream << endLabel << ":\n";
            std::string phiReg = "%t" + std::to_string(tempCounter++);
            irStream << "  " << phiReg << " = phi i1 [ true, %" << lhsPred << " ], [ " << rightCmp << ", %" << rightPred << " ]\n";
            currentBlockLabel = endLabel;
            std::string extReg = "%t" + std::to_string(tempCounter++);
            irStream << "  " << extReg << " = zext i1 " << phiReg << " to i32\n";
            currentValue = extReg;
            condResultReg = phiReg;
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
        // llvm.memset 内置函数声明，用于大数组零初始化
        irStream << "declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg)\n";
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

    // 获取系统函数的参数类型列表
    std::vector<std::string> CodeGenerator::getSystemFunctionParamTypes(const std::string &funcName)
    {
        std::vector<std::string> paramTypes;
        
        if (funcName == "getint" || funcName == "getch")
        {
            // 无参数
        }
        else if (funcName == "putint" || funcName == "putch")
        {
            paramTypes.push_back("i32");
        }
        else if (funcName == "getarray")
        {
            paramTypes.push_back("i32*");  // getarray 接收数组指针
        }
        else if (funcName == "putfloat")
        {
            paramTypes.push_back("float");
        }
        else if (funcName == "putarray")
        {
            paramTypes.push_back("i32");   // 第一个参数：数组长度
            paramTypes.push_back("i32*");  // 第二个参数：数组指针
        }
        else if (funcName == "getfarray")
        {
            paramTypes.push_back("float*");
        }
        else if (funcName == "putfarray")
        {
            paramTypes.push_back("i32");    // 数组长度
            paramTypes.push_back("float*"); // 数组指针
        }
        else if (funcName == "putf")
        {
            paramTypes.push_back("i8*");  // 格式字符串指针，可能有更多参数
        }
        else if (funcName == "_sysy_starttime" || funcName == "_sysy_stoptime")
        {
            paramTypes.push_back("i32");
        }
        
        return paramTypes;
    }

    // ========== Alloca Hoisting: 收集局部变量 ==========
    
    std::string CodeGenerator::getNextAllocaIrName(const std::string& varName)
    {
        // 从pendingAllocas中按顺序获取对应的irName
        if (currentAllocaIndex < pendingAllocas.size())
        {
            // 验证变量名匹配
            if (pendingAllocas[currentAllocaIndex].name == varName)
            {
                return pendingAllocas[currentAllocaIndex++].irName;
            }
            // 如果不匹配，可能是跳过了某些声明，搜索匹配的
            for (size_t i = currentAllocaIndex; i < pendingAllocas.size(); ++i)
            {
                if (pendingAllocas[i].name == varName)
                {
                    currentAllocaIndex = i + 1;
                    return pendingAllocas[i].irName;
                }
            }
        }
        // 找不到时返回默认名称（不应该发生）
        std::cerr << "Warning: Could not find alloca for variable '" << varName << "'" << std::endl;
        return "%" + varName;
    }
    
    void CodeGenerator::collectLocalVars(BlockStmtNode* block)
    {
        pendingAllocas.clear();
        allocaNameCounters.clear();
        collectingAllocas = true;
        collectLocalVarsFromBlock(block);
        collectingAllocas = false;
    }
    
    void CodeGenerator::collectLocalVarsFromBlock(BlockStmtNode* block)
    {
        if (!block) return;
        
        for (auto& item : block->blockItems)
        {
            if (!item) continue;
            
            // 处理声明
            if (item->isDecl && item->decl)
            {
                // 变量声明
                VarDeclNode* varDecl = dynamic_cast<VarDeclNode*>(item->decl.get());
                if (varDecl)
                {
                    for (auto& def : varDecl->varDefs)
                    {
                        if (!def) continue;
                        
                        LocalVarInfo info;
                        info.name = def->ident;
                        info.entry = nullptr; // 不在这里设置entry
                        info.isArray = !def->dims.empty();
                        
                        // 从AST获取维度
                        for (auto& dim : def->dims) {
                            if (dim) {
                                info.dimensions.push_back(evaluateConstExp(dim.get()));
                            }
                        }
                        
                        // 生成唯一的IR名称
                        if (allocaNameCounters.find(info.name) != allocaNameCounters.end()) {
                            int count = allocaNameCounters[info.name]++;
                            info.irName = "%" + info.name + std::to_string(count);
                        } else {
                            allocaNameCounters[info.name] = 1;
                            info.irName = "%" + info.name;
                        }
                        
                        pendingAllocas.push_back(info);
                    }
                }
                
                // 常量声明
                ConstDeclNode* constDecl = dynamic_cast<ConstDeclNode*>(item->decl.get());
                if (constDecl)
                {
                    for (auto& def : constDecl->constDefs)
                    {
                        if (!def) continue;
                        
                        LocalVarInfo info;
                        info.name = def->ident;
                        info.entry = nullptr; // 不在这里设置entry
                        info.isArray = !def->dims.empty();
                        
                        // 从AST获取维度
                        for (auto& dim : def->dims) {
                            if (dim) {
                                info.dimensions.push_back(evaluateConstExp(dim.get()));
                            }
                        }
                        
                        if (allocaNameCounters.find(info.name) != allocaNameCounters.end()) {
                            int count = allocaNameCounters[info.name]++;
                            info.irName = "%" + info.name + std::to_string(count);
                        } else {
                            allocaNameCounters[info.name] = 1;
                            info.irName = "%" + info.name;
                        }
                        
                        pendingAllocas.push_back(info);
                    }
                }
            }
            
            // 递归处理语句中的嵌套块
            if (item->stmt)
            {
                collectLocalVarsFromStmt(item->stmt.get());
            }
        }
    }
    
    void CodeGenerator::collectLocalVarsFromStmt(StmtNode* stmt)
    {
        if (!stmt) return;
        
        // Block语句
        BlockStmtNode* block = dynamic_cast<BlockStmtNode*>(stmt);
        if (block) {
            collectLocalVarsFromBlock(block);
            return;
        }
        
        // If语句
        IfStmtNode* ifStmt = dynamic_cast<IfStmtNode*>(stmt);
        if (ifStmt) {
            if (ifStmt->thenStmt) collectLocalVarsFromStmt(ifStmt->thenStmt.get());
            if (ifStmt->elseStmt) collectLocalVarsFromStmt(ifStmt->elseStmt.get());
            return;
        }
        
        // While语句
        WhileStmtNode* whileStmt = dynamic_cast<WhileStmtNode*>(stmt);
        if (whileStmt) {
            if (whileStmt->body) collectLocalVarsFromStmt(whileStmt->body.get());
            return;
        }
    }

} // namespace sysy


