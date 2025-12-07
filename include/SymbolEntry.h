#pragma once

#include "Nodes.h"  // 包含 ConstExpNode 的完整定义
#include <string>
#include <vector>
#include <memory>

namespace sysy {

/**
 * 符号类型枚举
 */
enum class SymbolType {
    VARIABLE,    // 变量
    CONSTANT,    // 常量
    FUNCTION,    // 函数
    PARAMETER    // 函数参数
};

/**
 * 数据类型枚举
 */
enum class DataType {
    INT,         // 整数类型
    VOID,        // void类型（仅用于函数返回类型）
    INT_ARRAY    // 整数数组类型
};

/**
 * 符号表条目基类
 */
class SymbolEntry {
public:
    std::string name;
    SymbolType symbolType;
    DataType dataType;
    bool isConst;
    int scopeLevel;
    
    SymbolEntry(const std::string& name, SymbolType type, DataType dataType, bool isConst, int scopeLevel)
        : name(name), symbolType(type), dataType(dataType), isConst(isConst), scopeLevel(scopeLevel) {}
    
    virtual ~SymbolEntry() = default;
    
    std::string getName() const { return name; }
    SymbolType getSymbolType() const { return symbolType; }
    DataType getDataType() const { return dataType; }
    bool isConstant() const { return isConst; }
    int getScopeLevel() const { return scopeLevel; }
};

/**
 * 变量条目
 */
class VariableEntry : public SymbolEntry {
public:
    bool isArray;
    std::vector<int> dimensions;  // 编译时已知的维度大小
    std::vector<ConstExpNode*> dimExprs;  // 数组维度表达式列表
    bool isInitialized;
    std::string irName;  // 对应的LLVM IR变量名（如 "%a"）
    
    VariableEntry(const std::string& name, DataType dataType, bool isConst, int scopeLevel)
        : SymbolEntry(name, isConst ? SymbolType::CONSTANT : SymbolType::VARIABLE, 
                     dataType, isConst, scopeLevel),
          isArray(false), isInitialized(false), irName("") {}
    
    bool isArrayType() const { return isArray; }
    int getArraySize() const;
    int getDimensionCount() const { return static_cast<int>(dimensions.size()); }
    int getDimension(int index) const { return dimensions[index]; }
};

/**
 * 函数条目
 */
class FunctionEntry : public SymbolEntry {
public:
    DataType returnType;
    std::vector<std::unique_ptr<VariableEntry>> parameters;  // 参数列表
    class SymbolTable* localSymbolTable;  // 函数局部符号表
    std::string irName;  // 对应的LLVM IR函数名（如 "@main"）
    bool isDefined;
    
    FunctionEntry(const std::string& name, DataType returnType, int scopeLevel)
        : SymbolEntry(name, SymbolType::FUNCTION, returnType, false, scopeLevel),
          returnType(returnType), localSymbolTable(nullptr), 
          irName(""), isDefined(false) {}
    
    DataType getReturnType() const { return returnType; }
    int getParameterCount() const { return static_cast<int>(parameters.size()); }
    VariableEntry* getParameter(int index) const;
    void addParameter(std::unique_ptr<VariableEntry> param);
    class SymbolTable* getLocalSymbolTable() const { return localSymbolTable; }
    void setLocalSymbolTable(class SymbolTable* table) { localSymbolTable = table; }
};

/**
 * 参数条目
 */
class ParameterEntry : public VariableEntry {
public:
    int paramIndex;
    bool isArrayParam;
    std::vector<int> arrayDims;  // 数组参数的后续维度
    
    ParameterEntry(const std::string& name, DataType dataType, int scopeLevel, int paramIndex)
        : VariableEntry(name, dataType, false, scopeLevel),
          paramIndex(paramIndex), isArrayParam(false) {}
    
    int getParamIndex() const { return paramIndex; }
    bool isArrayParameter() const { return isArrayParam; }
};

} // namespace sysy

