# 从 ParseTree 到 LLVM IR 的完整调用流程

本文档详细描述了 SysY 编译器从 ANTLR ParseTree 到 LLVM IR 的完整调用流程，包括各个阶段的职责和调用关系。

## 整体架构

编译器采用经典的编译流程设计，主要包含以下几个阶段：

```
源代码 → 词法分析 → 语法分析 → AST构建 → 语义分析 → 代码生成 → LLVM IR
```

## 详细调用流程

### 1. 入口：main 函数

**文件：** `src/main.cpp`

```cpp
int main(int argc, const char *argv[]) {
    sysy::Compiler compiler;
    compiler.compile(argv[1], argv[2]);
}
```

**职责：**
- 创建编译器实例
- 调用 `compile()` 方法开始编译流程

---

### 2. 编译器主流程：Compiler::compile()

**文件：** `src/Compiler/Compiler.cpp`

```cpp
bool Compiler::compile(const std::string& inputFile, const std::string& outputFile) {
    // 1. 解析：从源代码构建AST
    auto ast = parse(inputFile);
    
    // 2. 语义分析：构建符号表，进行类型检查
    semanticAnalysis(ast.get());
    
    // 3. 代码生成：生成LLVM IR
    generateCode(ast.get());
    
    // 4. 输出IR到文件
    outputIR(outputFile);
}
```

**职责：**
- 协调各个编译阶段
- 管理编译流程的顺序执行

---

### 3. 阶段一：解析（Parse）

#### 3.1 Compiler::parse()

**文件：** `src/Compiler/Compiler.cpp`

```cpp
std::unique_ptr<CompUnitNode> Compiler::parse(const std::string& inputFile) {
    // 1. 读取输入文件
    std::ifstream file(inputFile);
    
    // 2. 创建ANTLR输入流
    antlr4::ANTLRInputStream input(file);
    
    // 3. 创建词法分析器
    SysYLexer lexer(&input);
    
    // 4. 创建词法符号流
    antlr4::CommonTokenStream tokens(&lexer);
    
    // 5. 创建语法分析器
    SysYParser parser(&tokens);
    
    // 6. 获取ParseTree根节点
    antlr4::tree::ParseTree* tree = parser.compUnit();
    
    // 7. 使用ASTBuilder构建AST
    return astBuilder->build(tree);
}
```

**调用链：**
```
Compiler::parse()
  └─> ASTBuilder::build(ParseTree*)
      └─> ASTBuilder::buildCompUnit(ParseTree*)
          └─> 递归调用各种build方法构建AST节点
```

**职责：**
- 使用 ANTLR 进行词法和语法分析
- 获取 ParseTree
- 调用 ASTBuilder 构建 AST

---

#### 3.2 ASTBuilder::build()

**文件：** `src/ASTBuilder/ASTBuilder.cpp`

```cpp
std::unique_ptr<CompUnitNode> ASTBuilder::build(antlr4::tree::ParseTree* tree) {
    return buildCompUnit(tree);
}
```

**职责：**
- 作为 AST 构建的入口点
- 分发到具体的构建方法

---

#### 3.3 ASTBuilder::buildCompUnit()

**文件：** `src/ASTBuilder/ASTBuilder.cpp`

```cpp
std::unique_ptr<CompUnitNode> ASTBuilder::buildCompUnit(antlr4::tree::ParseTree* tree) {
    auto node = std::make_unique<CompUnitNode>();
    
    // 遍历ParseTree的子节点
    for (auto child : tree->children) {
        if (是声明节点) {
            node->decls.push_back(buildDecl(child));
        } else if (是函数定义节点) {
            node->funcDefs.push_back(buildFuncDef(child));
        }
    }
    
    return node;
}
```

**调用链示例（构建函数定义）：**
```
ASTBuilder::buildCompUnit()
  ├─> ASTBuilder::buildDecl() / buildFuncDef()
  │   └─> ASTBuilder::buildConstDecl() / buildVarDecl() / buildFuncDef()
  │       └─> ASTBuilder::buildConstDef() / buildVarDef()
  │           └─> ASTBuilder::buildConstInitVal() / buildInitVal()
  │               └─> ASTBuilder::buildConstExp() / buildExp()
  │                   └─> 递归构建表达式节点
  └─> 返回 CompUnitNode*
```

**职责：**
- 从 ParseTree 节点提取信息
- 创建对应的 AST 节点
- 递归构建子节点
- 返回完整的 AST

---

### 4. 阶段二：语义分析（Semantic Analysis）

#### 4.1 Compiler::semanticAnalysis()

**文件：** `src/Compiler/Compiler.cpp`

```cpp
void Compiler::semanticAnalysis(CompUnitNode* ast) {
    semanticAnalyzer->analyze(ast);
}
```

**调用链：**
```
Compiler::semanticAnalysis()
  └─> SemanticAnalyzer::analyze(CompUnitNode*)
      └─> CompUnitNode::accept(SemanticAnalyzer*)
          └─> SemanticAnalyzer::visitCompUnit(CompUnitNode*)
              └─> 递归访问所有AST节点
```

**职责：**
- 启动语义分析流程
- 使用访问者模式遍历 AST

---

#### 4.2 SemanticAnalyzer::analyze()

**文件：** `src/SemanticAnalyzer/SemanticAnalyzer.cpp`

```cpp
void SemanticAnalyzer::analyze(CompUnitNode* root) {
    root->accept(this);
}
```

**职责：**
- 启动访问者模式遍历
- 从根节点开始访问整个 AST

---

#### 4.3 访问者模式遍历

**访问流程示例（处理函数定义）：**

```
SemanticAnalyzer::visitCompUnit(CompUnitNode*)
  ├─> 遍历 decls，调用 visitConstDecl() / visitVarDecl()
  │   ├─> 构建符号表条目
  │   ├─> 插入到符号表
  │   └─> 检查重复定义
  │
  └─> 遍历 funcDefs，调用 visitFuncDef()
      ├─> 创建函数符号表条目
      ├─> 插入到全局符号表
      ├─> 进入函数作用域
      │   ├─> 处理函数参数
      │   │   └─> visitFuncFParam()
      │   │       └─> 创建参数符号表条目
      │   │
      │   └─> 处理函数体
      │       └─> visitBlockStmt()
      │           └─> 遍历 blockItems
      │               ├─> visitBlockItem()
      │               │   ├─> 如果是声明：visitConstDecl() / visitVarDecl()
      │               │   └─> 如果是语句：visitAssignStmt() / visitIfStmt() / ...
      │               └─> 类型检查和语义验证
      │
      └─> 退出函数作用域
```

**职责：**
- 构建符号表（全局符号表、函数局部符号表）
- 检查重复定义
- 检查未定义引用
- 进行类型检查
- 验证语义正确性（如 break/continue 必须在循环内）

---

### 5. 阶段三：代码生成（Code Generation）

#### 5.1 Compiler::generateCode()

**文件：** `src/Compiler/Compiler.cpp`

```cpp
void Compiler::generateCode(CompUnitNode* ast) {
    codeGenerator->generate(ast);
}
```

**调用链：**
```
Compiler::generateCode()
  └─> CodeGenerator::generate(CompUnitNode*)
      └─> CompUnitNode::accept(CodeGenerator*)
          └─> CodeGenerator::visitCompUnit(CompUnitNode*)
              └─> 递归访问所有AST节点并生成LLVM IR
```

**职责：**
- 启动代码生成流程
- 使用访问者模式遍历 AST 并生成 LLVM IR

---

#### 5.2 CodeGenerator::generate()

**文件：** `src/CodeGen/CodeGenerator.cpp`

```cpp
void CodeGenerator::generate(CompUnitNode* root) {
    root->accept(this);
}
```

**职责：**
- 启动访问者模式遍历
- 从根节点开始生成 LLVM IR

---

#### 5.3 代码生成流程示例

**生成全局变量：**

```
CodeGenerator::visitCompUnit(CompUnitNode*)
  └─> 遍历 decls
      └─> CodeGenerator::visitVarDecl(VarDeclNode*)
          └─> 遍历 varDefs
              └─> CodeGenerator::visitVarDef(VarDefNode*)
                  ├─> 创建全局变量：module->getOrInsertGlobal()
                  ├─> 设置初始值（如果有）
                  │   └─> CodeGenerator::visitInitVal(InitValNode*)
                  │       └─> 生成初始化表达式
                  └─> 将 llvm::Value* 存储到符号表条目
```

**生成函数：**

```
CodeGenerator::visitFuncDef(FuncDefNode*)
  ├─> 创建函数类型：llvm::FunctionType::get()
  ├─> 创建函数：llvm::Function::Create()
  ├─> 创建函数入口基本块：llvm::BasicBlock::Create()
  ├─> 设置 IRBuilder 插入点
  │
  ├─> 处理函数参数
  │   └─> CodeGenerator::visitFuncFParam(FuncFParamNode*)
  │       ├─> 创建参数 alloca
  │       ├─> 存储参数值
  │       └─> 将 llvm::Value* 存储到参数符号表条目
  │
  └─> 生成函数体
      └─> CodeGenerator::visitBlockStmt(BlockStmtNode*)
          └─> 遍历 blockItems
              └─> CodeGenerator::visitBlockItem(BlockItemNode*)
                  ├─> 如果是声明：生成局部变量 alloca
                  └─> 如果是语句：生成对应IR
                      ├─> visitAssignStmt() → 生成 store 指令
                      ├─> visitIfStmt() → 生成条件跳转
                      ├─> visitWhileStmt() → 生成循环结构
                      └─> visitReturnStmt() → 生成 return 指令
```

**生成表达式：**

```
CodeGenerator::visitAddExp(AddExpNode*)
  ├─> 递归处理左操作数：left->accept(this)
  │   └─> 返回 llvm::Value* (左操作数的值)
  │
  ├─> 递归处理右操作数：right->accept(this)
  │   └─> 返回 llvm::Value* (右操作数的值)
  │
  └─> 根据操作符生成IR指令
      ├─> op == PLUS → builder->CreateAdd(leftVal, rightVal)
      └─> op == MINUS → builder->CreateSub(leftVal, rightVal)
```

**生成控制流（if 语句）：**

```
CodeGenerator::visitIfStmt(IfStmtNode*)
  ├─> 生成条件表达式
  │   └─> CodeGenerator::visitCond(CondNode*)
  │       └─> 返回 llvm::Value* (条件值，i1类型)
  │
  ├─> 创建基本块
  │   ├─> thenBB = llvm::BasicBlock::Create()
  │   ├─> elseBB = llvm::BasicBlock::Create() (如果有else)
  │   └─> mergeBB = llvm::BasicBlock::Create()
  │
  ├─> 生成条件跳转
  │   └─> builder->CreateCondBr(condVal, thenBB, elseBB)
  │
  ├─> 生成 then 分支
  │   ├─> builder->SetInsertPoint(thenBB)
  │   └─> thenStmt->accept(this)
  │
  ├─> 生成 else 分支（如果有）
  │   ├─> builder->SetInsertPoint(elseBB)
  │   └─> elseStmt->accept(this)
  │
  └─> 生成合并基本块
      └─> builder->SetInsertPoint(mergeBB)
```

**生成循环（while 语句）：**

```
CodeGenerator::visitWhileStmt(WhileStmtNode*)
  ├─> 保存当前 break/continue 目标
  │   ├─> oldBreak = currentBreakTarget
  │   └─> oldContinue = currentContinueTarget
  │
  ├─> 创建基本块
  │   ├─> condBB = llvm::BasicBlock::Create()  // 条件判断
  │   ├─> bodyBB = llvm::BasicBlock::Create()  // 循环体
  │   └─> endBB = llvm::BasicBlock::Create()   // 循环结束
  │
  ├─> 设置 break/continue 目标
  │   ├─> currentBreakTarget = endBB
  │   └─> currentContinueTarget = condBB
  │
  ├─> 跳转到条件判断
  │   └─> builder->CreateBr(condBB)
  │
  ├─> 生成条件判断
  │   ├─> builder->SetInsertPoint(condBB)
  │   └─> CodeGenerator::visitCond(CondNode*)
  │       └─> builder->CreateCondBr(condVal, bodyBB, endBB)
  │
  ├─> 生成循环体
  │   ├─> builder->SetInsertPoint(bodyBB)
  │   └─> body->accept(this)
  │
  ├─> 跳转到条件判断（continue）
  │   └─> builder->CreateBr(condBB)
  │
  └─> 恢复 break/continue 目标
      ├─> currentBreakTarget = oldBreak
      └─> currentContinueTarget = oldContinue
```

---

### 6. 阶段四：输出 IR

#### 6.1 Compiler::outputIR()

**文件：** `src/Compiler/Compiler.cpp`

```cpp
void Compiler::outputIR(const std::string& outputFile) {
    std::error_code EC;
    llvm::raw_fd_ostream OS(outputFile, EC);
    if (EC) {
        std::cerr << "Error: Cannot open output file: " << outputFile << std::endl;
        return;
    }
    
    module->print(OS, nullptr);
    OS.flush();
}
```

**职责：**
- 将生成的 LLVM IR 模块输出到文件
- 使用 LLVM 的打印功能生成可读的 IR 文本

---

## 完整调用流程图

```
main()
  │
  └─> Compiler::compile()
      │
      ├─> [阶段1] Compiler::parse()
      │   │
      │   ├─> 读取文件
      │   ├─> 创建 ANTLR 输入流
      │   ├─> SysYLexer (词法分析)
      │   ├─> SysYParser (语法分析)
      │   │   └─> 返回 ParseTree*
      │   │
      │   └─> ASTBuilder::build(ParseTree*)
      │       └─> 递归构建 AST 节点
      │           └─> 返回 CompUnitNode*
      │
      ├─> [阶段2] Compiler::semanticAnalysis()
      │   │
      │   └─> SemanticAnalyzer::analyze(CompUnitNode*)
      │       └─> 使用访问者模式遍历 AST
      │           ├─> 构建符号表
      │           ├─> 类型检查
      │           └─> 语义验证
      │
      ├─> [阶段3] Compiler::generateCode()
      │   │
      │   └─> CodeGenerator::generate(CompUnitNode*)
      │       └─> 使用访问者模式遍历 AST
      │           ├─> 生成全局变量 IR
      │           ├─> 生成函数 IR
      │           ├─> 生成表达式 IR
      │           └─> 生成控制流 IR
      │
      └─> [阶段4] Compiler::outputIR()
          │
          └─> 将 LLVM IR 模块输出到文件
```

---

## 关键设计模式

### 1. 访问者模式（Visitor Pattern）

**用途：** 遍历 AST 并执行操作（语义分析、代码生成）

**实现：**
- `ASTVisitor` 基类定义所有访问方法
- `SemanticAnalyzer` 和 `CodeGenerator` 继承并实现访问方法
- 每个 AST 节点实现 `accept()` 方法，调用对应的访问方法

**优势：**
- 将算法与数据结构分离
- 易于添加新的遍历操作（如优化、调试输出）

### 2. 符号表管理

**用途：** 管理作用域和符号信息

**实现：**
- `SymbolTable` 管理单个作用域的符号
- `SymbolTableManager` 管理整个程序的符号表层次结构
- 支持嵌套作用域和符号查找

### 3. 智能指针管理内存

**用途：** 自动管理 AST 节点和符号表条目的生命周期

**实现：**
- 使用 `std::unique_ptr` 管理 AST 节点
- 使用 `std::unique_ptr` 管理符号表条目
- 避免内存泄漏

---

## 示例：编译简单程序

**输入（test.sy）：**
```c
int main() {
    int a = 10;
    return a;
}
```

**调用流程：**

1. **解析阶段：**
   - `parse()` → 创建 `CompUnitNode`
   - 包含一个 `FuncDefNode`（main 函数）
   - 函数体包含一个 `VarDefNode`（变量 a）和一个 `ReturnStmtNode`

2. **语义分析阶段：**
   - `visitCompUnit()` → 创建全局符号表
   - `visitFuncDef()` → 创建函数符号表条目，进入函数作用域
   - `visitVarDef()` → 创建变量符号表条目
   - `visitReturnStmt()` → 检查返回类型匹配

3. **代码生成阶段：**
   - `visitFuncDef()` → 创建 LLVM 函数
   - `visitVarDef()` → 创建 alloca 指令，生成初始化代码
   - `visitReturnStmt()` → 生成 return 指令

4. **输出阶段：**
   - 将生成的 LLVM IR 写入文件

**生成的 LLVM IR（test.ll）：**
```llvm
define i32 @main() {
entry:
  %a = alloca i32
  store i32 10, i32* %a
  %0 = load i32, i32* %a
  ret i32 %0
}
```

---

## 总结

整个编译流程从 ParseTree 到 LLVM IR 经过以下关键步骤：

1. **AST 构建**：从 ParseTree 递归构建 AST 节点
2. **语义分析**：使用访问者模式遍历 AST，构建符号表，进行类型检查
3. **代码生成**：使用访问者模式遍历 AST，生成 LLVM IR 指令
4. **IR 输出**：将生成的 IR 模块写入文件

每个阶段都有清晰的职责划分，通过访问者模式实现了代码的解耦和可扩展性。

