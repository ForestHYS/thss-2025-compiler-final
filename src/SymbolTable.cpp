#include "SymbolTable.h"
#include <iostream>

namespace sysy {

// SymbolTable实现
SymbolTable::~SymbolTable() {
    // 清理子作用域
    for (auto* child : children) {
        delete child;
    }
}

bool SymbolTable::insert(std::unique_ptr<SymbolEntry> entry) {
    if (symbols.find(entry->name) != symbols.end()) {
        return false;  // 符号已存在
    }
    symbols[entry->name] = std::move(entry);
    return true;
}

SymbolEntry* SymbolTable::lookup(const std::string& name) const {
    auto it = symbols.find(name);
    if (it != symbols.end()) {
        return it->second.get();
    }
    return nullptr;
}

SymbolEntry* SymbolTable::lookupAll(const std::string& name) const {
    const SymbolTable* current = this;
    while (current != nullptr) {
        SymbolEntry* entry = current->lookup(name);
        if (entry != nullptr) {
            return entry;
        }
        current = current->parent;
    }
    return nullptr;
}

bool SymbolTable::remove(const std::string& name) {
    return symbols.erase(name) > 0;
}

bool SymbolTable::contains(const std::string& name) const {
    return symbols.find(name) != symbols.end();
}

bool SymbolTable::containsAll(const std::string& name) const {
    return lookupAll(name) != nullptr;
}

SymbolTable* SymbolTable::createChildScope() {
    auto* child = new SymbolTable(this, scopeLevel + 1);
    children.push_back(child);
    return child;
}

SymbolTable* SymbolTable::exitScope() {
    return parent;
}

std::vector<SymbolEntry*> SymbolTable::getAllSymbols() const {
    std::vector<SymbolEntry*> result;
    for (const auto& pair : symbols) {
        result.push_back(pair.second.get());
    }
    return result;
}

void SymbolTable::print() const {
    std::cout << "=== SymbolTable (Level " << scopeLevel << ") ===" << std::endl;
    for (const auto& pair : symbols) {
        std::cout << "  " << pair.first << " (Type: " 
                  << static_cast<int>(pair.second->getSymbolType()) << ")" << std::endl;
    }
    std::cout << std::endl;
}

// SymbolTableManager实现
SymbolTableManager::SymbolTableManager() 
    : globalTable(std::make_unique<SymbolTable>(nullptr, 0)),
      currentTable(globalTable.get()),
      functionTable(nullptr) {
}

SymbolTableManager::~SymbolTableManager() = default;

void SymbolTableManager::enterScope() {
    currentTable = currentTable->createChildScope();
}

void SymbolTableManager::exitScope() {
    if (currentTable != globalTable.get()) {
        currentTable = currentTable->exitScope();
    }
}

bool SymbolTableManager::enterExistingChildScope() {
    // 获取当前作用域下一个要访问的子作用域索引
    // childIndexStack 记录了从根到当前位置，每一层已经访问到了第几个子节点
    
    // 检查是否有子作用域可以进入
    if (currentTable->children.empty()) {
        return false;
    }
    
    // 确定要进入哪个子作用域
    // 如果 childIndexStack 的大小等于当前层级，需要获取下一个索引
    size_t currentDepth = childIndexStack.size();
    size_t childIndex = 0;
    
    // 使用栈来记录每一层的下一个子节点索引
    if (currentDepth < childIndexStack.size()) {
        // 不应该发生
        return false;
    }
    
    // 在当前层级，检查是否还有未访问的子节点
    // 使用一个 map 来记录每个符号表节点的下一个子节点索引
    auto it = nextChildIndexMap.find(currentTable);
    if (it != nextChildIndexMap.end()) {
        childIndex = it->second;
    } else {
        childIndex = 0;
        nextChildIndexMap[currentTable] = 0;
    }
    
    if (childIndex >= currentTable->children.size()) {
        return false;  // 没有更多子作用域
    }
    
    // 更新索引
    nextChildIndexMap[currentTable] = childIndex + 1;
    
    // 进入子作用域
    childIndexStack.push_back(childIndex);
    currentTable = currentTable->children[childIndex];
    
    return true;
}

void SymbolTableManager::exitExistingScope() {
    if (currentTable != globalTable.get() && currentTable->parent != nullptr) {
        currentTable = currentTable->parent;
        if (!childIndexStack.empty()) {
            childIndexStack.pop_back();
        }
    }
}

void SymbolTableManager::enterFunction(FunctionEntry* func) {
    if (func->localSymbolTable == nullptr) {
        func->setLocalSymbolTable(new SymbolTable(globalTable.get(), 1));
    }
    functionTable = func->localSymbolTable;
    currentTable = functionTable;
    
    // 重置子作用域遍历状态
    childIndexStack.clear();
    nextChildIndexMap.clear();
}

void SymbolTableManager::exitFunction() {
    currentTable = globalTable.get();
    functionTable = nullptr;
}

SymbolEntry* SymbolTableManager::lookup(const std::string& name) const {
    return currentTable->lookupAll(name);
}

SymbolEntry* SymbolTableManager::lookupGlobal(const std::string& name) const {
    return globalTable->lookup(name);
}

FunctionEntry* SymbolTableManager::lookupFunction(const std::string& name) const {
    SymbolEntry* entry = globalTable->lookup(name);
    if (entry != nullptr && entry->getSymbolType() == SymbolType::FUNCTION) {
        return static_cast<FunctionEntry*>(entry);
    }
    return nullptr;
}

VariableEntry* SymbolTableManager::lookupVariable(const std::string& name) const {
    SymbolEntry* entry = lookup(name);
    if (entry != nullptr && 
        (entry->getSymbolType() == SymbolType::VARIABLE || 
         entry->getSymbolType() == SymbolType::CONSTANT ||
         entry->getSymbolType() == SymbolType::PARAMETER)) {
        return static_cast<VariableEntry*>(entry);
    }
    return nullptr;
}

bool SymbolTableManager::insertGlobal(std::unique_ptr<SymbolEntry> entry) {
    return globalTable->insert(std::move(entry));
}

bool SymbolTableManager::insertLocal(std::unique_ptr<SymbolEntry> entry) {
    return currentTable->insert(std::move(entry));
}

bool SymbolTableManager::insertFunction(std::unique_ptr<FunctionEntry> func) {
    return globalTable->insert(std::unique_ptr<SymbolEntry>(func.release()));
}

bool SymbolTableManager::insertVariable(std::unique_ptr<VariableEntry> var) {
    return currentTable->insert(std::unique_ptr<SymbolEntry>(var.release()));
}

bool SymbolTableManager::insertParameter(std::unique_ptr<ParameterEntry> param) {
    return currentTable->insert(std::unique_ptr<SymbolEntry>(param.release()));
}

bool SymbolTableManager::checkTypeCompatibility(DataType t1, DataType t2) {
    return t1 == t2;
}

bool SymbolTableManager::isArrayType(DataType type) {
    return type == DataType::INT_ARRAY;
}

DataType SymbolTableManager::getArrayElementType(DataType type) {
    if (isArrayType(type)) {
        return DataType::INT;
    }
    return type;
}

// FunctionEntry方法实现
VariableEntry* FunctionEntry::getParameter(int index) const {
    if (index >= 0 && index < static_cast<int>(parameters.size())) {
        return parameters[index].get();
    }
    return nullptr;
}

void FunctionEntry::addParameter(std::unique_ptr<VariableEntry> param) {
    parameters.push_back(std::move(param));
}

// VariableEntry方法实现
int VariableEntry::getArraySize() const {
    if (!isArray) return 1;
    int size = 1;
    for (int dim : dimensions) {
        size *= dim;
    }
    return size;
}

} // namespace sysy

