#pragma once

#include "SymbolEntry.h"
#include <unordered_map>
#include <vector>
#include <memory>

namespace sysy {

/**
 * 符号表类
 * 管理作用域内的符号，支持嵌套作用域和符号查找
 */
class SymbolTable {
public:
    std::unordered_map<std::string, std::unique_ptr<SymbolEntry>> symbols;
    SymbolTable* parent;
    int scopeLevel;
    std::vector<SymbolTable*> children;
    
    SymbolTable(SymbolTable* parent = nullptr, int scopeLevel = 0)
        : parent(parent), scopeLevel(scopeLevel) {}
    
    ~SymbolTable();
    
    // 符号管理
    bool insert(std::unique_ptr<SymbolEntry> entry);
    SymbolEntry* lookup(const std::string& name) const;
    SymbolEntry* lookupAll(const std::string& name) const;
    bool remove(const std::string& name);
    bool contains(const std::string& name) const;
    bool containsAll(const std::string& name) const;
    
    // 作用域管理
    SymbolTable* getParent() const { return parent; }
    int getScopeLevel() const { return scopeLevel; }
    SymbolTable* createChildScope();
    SymbolTable* exitScope();
    
    // 遍历和调试
    std::vector<SymbolEntry*> getAllSymbols() const;
    void print() const;
};

/**
 * 符号表管理器
 * 管理整个程序的符号表层次结构
 */
class SymbolTableManager {
public:
    std::unique_ptr<SymbolTable> globalTable;
    SymbolTable* currentTable;
    SymbolTable* functionTable;  // 当前函数局部符号表
    
    SymbolTableManager();
    ~SymbolTableManager();
    
    // 作用域管理
    void enterScope();
    void exitScope();
    void enterFunction(FunctionEntry* func);
    void exitFunction();
    SymbolTable* getCurrentScope() const { return currentTable; }
    SymbolTable* getGlobalScope() const { return globalTable.get(); }
    
    // 符号查找
    SymbolEntry* lookup(const std::string& name) const;
    SymbolEntry* lookupGlobal(const std::string& name) const;
    FunctionEntry* lookupFunction(const std::string& name) const;
    VariableEntry* lookupVariable(const std::string& name) const;
    
    // 符号插入
    bool insertGlobal(std::unique_ptr<SymbolEntry> entry);
    bool insertLocal(std::unique_ptr<SymbolEntry> entry);
    bool insertFunction(std::unique_ptr<FunctionEntry> func);
    bool insertVariable(std::unique_ptr<VariableEntry> var);
    bool insertParameter(std::unique_ptr<ParameterEntry> param);
    
    // 类型检查辅助
    static bool checkTypeCompatibility(DataType t1, DataType t2);
    static bool isArrayType(DataType type);
    static DataType getArrayElementType(DataType type);
};

} // namespace sysy

