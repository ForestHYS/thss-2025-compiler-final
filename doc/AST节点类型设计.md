# SysY 编译器 AST 节点类型设计

## 概述

本文档定义了从 SysY 语言到 LLVM IR 编译器所需的抽象语法树（AST）节点类型。AST 节点类型基于 SysY 语言的语法规则设计，用于表示程序的完整结构。

## 节点类型层次结构

### 1. 根节点

#### `CompUnitNode` - 编译单元节点
表示整个程序的根节点。

**属性：**
- `decls: vector<DeclNode*>` - 全局声明列表（常量声明、变量声明）
- `funcDefs: vector<FuncDefNode*>` - 函数定义列表

---

### 2. 声明节点

#### `DeclNode` - 声明节点（抽象基类）
所有声明节点的基类。

**子类：**
- `ConstDeclNode` - 常量声明
- `VarDeclNode` - 变量声明

---

#### `ConstDeclNode` - 常量声明节点
表示常量声明，如 `const int a = 1;`

**属性：**
- `bType: BType` - 基本类型（目前仅支持 `INT`）
- `constDefs: vector<ConstDefNode*>` - 常量定义列表

---

#### `VarDeclNode` - 变量声明节点
表示变量声明，如 `int a, b[10];`

**属性：**
- `bType: BType` - 基本类型（目前仅支持 `INT`）
- `varDefs: vector<VarDefNode*>` - 变量定义列表

---

#### `ConstDefNode` - 常量定义节点
表示单个常量定义，如 `const int a[2][3] = {{1,2,3},{4,5,6}};`

**属性：**
- `ident: string` - 标识符名称
- `dims: vector<ConstExpNode*>` - 数组维度表达式列表（空表示非数组）
- `initVal: ConstInitValNode*` - 初始化值

---

#### `VarDefNode` - 变量定义节点
表示单个变量定义，如 `int a[10] = {1,2,3};` 或 `int b;`

**属性：**
- `ident: string` - 标识符名称
- `dims: vector<ConstExpNode*>` - 数组维度表达式列表（空表示非数组）
- `initVal: InitValNode*` - 初始化值（可选，nullptr 表示未初始化）

---

#### `ConstInitValNode` - 常量初始化值节点
表示常量的初始化值。

**属性：**
- `isScalar: bool` - 是否为标量（true）或数组（false）
- `scalarVal: ConstExpNode*` - 标量值（当 isScalar 为 true 时使用）
- `arrayVals: vector<ConstInitValNode*>` - 数组元素列表（当 isScalar 为 false 时使用）

---

#### `InitValNode` - 变量初始化值节点
表示变量的初始化值。

**属性：**
- `isScalar: bool` - 是否为标量（true）或数组（false）
- `scalarVal: ExpNode*` - 标量值（当 isScalar 为 true 时使用）
- `arrayVals: vector<InitValNode*>` - 数组元素列表（当 isScalar 为 false 时使用）

---

### 3. 函数节点

#### `FuncDefNode` - 函数定义节点
表示函数定义，如 `int add(int a, int b) { ... }`

**属性：**
- `funcType: FuncType` - 函数返回类型（`VOID` 或 `INT`）
- `ident: string` - 函数名称
- `params: vector<FuncFParamNode*>` - 形式参数列表
- `block: BlockNode*` - 函数体

---

#### `FuncFParamNode` - 函数形式参数节点
表示函数参数，如 `int a[]` 或 `int a[][10]`

**属性：**
- `bType: BType` - 基本类型
- `ident: string` - 参数名称
- `isArray: bool` - 是否为数组
- `dims: vector<ExpNode*>` - 数组维度表达式列表（第一维为空，后续维度有表达式）

---

### 4. 语句节点

#### `StmtNode` - 语句节点（抽象基类）
所有语句节点的基类。

**子类：**
- `AssignStmtNode` - 赋值语句
- `ExpStmtNode` - 表达式语句
- `BlockStmtNode` - 块语句
- `IfStmtNode` - 条件语句
- `WhileStmtNode` - 循环语句
- `BreakStmtNode` - break 语句
- `ContinueStmtNode` - continue 语句
- `ReturnStmtNode` - return 语句

---

#### `AssignStmtNode` - 赋值语句节点
表示赋值语句，如 `a = 1;` 或 `arr[0] = 2;`

**属性：**
- `lVal: LValNode*` - 左值（赋值目标）
- `exp: ExpNode*` - 右值表达式

---

#### `ExpStmtNode` - 表达式语句节点
表示表达式语句，如 `func();` 或 `;`

**属性：**
- `exp: ExpNode*` - 表达式（可选，nullptr 表示空语句）

---

#### `BlockStmtNode` - 块语句节点
表示代码块，如 `{ int a; a = 1; }`

**属性：**
- `blockItems: vector<BlockItemNode*>` - 块内项目列表（声明或语句）

---

#### `BlockItemNode` - 块项目节点
表示块内的一个项目（声明或语句）。

**属性：**
- `isDecl: bool` - 是否为声明（true）或语句（false）
- `decl: DeclNode*` - 声明节点（当 isDecl 为 true 时使用）
- `stmt: StmtNode*` - 语句节点（当 isDecl 为 false 时使用）

---

#### `IfStmtNode` - 条件语句节点
表示 if 语句，如 `if (a > 0) { ... } else { ... }`

**属性：**
- `cond: CondNode*` - 条件表达式
- `thenStmt: StmtNode*` - then 分支语句
- `elseStmt: StmtNode*` - else 分支语句（可选，nullptr 表示无 else）

---

#### `WhileStmtNode` - 循环语句节点
表示 while 循环，如 `while (i < 10) { ... }`

**属性：**
- `cond: CondNode*` - 循环条件表达式
- `body: StmtNode*` - 循环体语句

---

#### `BreakStmtNode` - break 语句节点
表示 break 语句。

**属性：** 无

---

#### `ContinueStmtNode` - continue 语句节点
表示 continue 语句。

**属性：** 无

---

#### `ReturnStmtNode` - return 语句节点
表示 return 语句，如 `return 0;` 或 `return;`

**属性：**
- `exp: ExpNode*` - 返回值表达式（可选，nullptr 表示无返回值）

---

### 5. 表达式节点

#### `ExpNode` - 表达式节点（抽象基类）
所有表达式节点的基类。

**子类：**
- `AddExpNode` - 加减表达式
- `MulExpNode` - 乘除模表达式
- `UnaryExpNode` - 一元表达式
- `PrimaryExpNode` - 基本表达式
- `LValNode` - 左值表达式
- `NumberNode` - 数字字面量
- `FuncCallNode` - 函数调用表达式
- `RelExpNode` - 关系表达式
- `EqExpNode` - 相等表达式
- `LAndExpNode` - 逻辑与表达式
- `LOrExpNode` - 逻辑或表达式

---

#### `AddExpNode` - 加减表达式节点
表示加减运算，如 `a + b - c`

**属性：**
- `op: BinaryOp` - 操作符（`PLUS` 或 `MINUS`）
- `left: AddExpNode*` - 左操作数（可为 nullptr，表示单操作数）
- `right: MulExpNode*` - 右操作数

**注意：** 为简化实现，可以仅存储 `MulExpNode*` 列表和操作符列表，或使用递归结构。

---

#### `MulExpNode` - 乘除模表达式节点
表示乘除模运算，如 `a * b / c % d`

**属性：**
- `op: BinaryOp` - 操作符（`MUL`、`DIV` 或 `MOD`）
- `left: MulExpNode*` - 左操作数（可为 nullptr，表示单操作数）
- `right: UnaryExpNode*` - 右操作数

---

#### `UnaryExpNode` - 一元表达式节点
表示一元运算，如 `-a`、`!b` 或 `+c`

**属性：**
- `op: UnaryOp` - 操作符（`PLUS`、`MINUS` 或 `NOT`）
- `operand: UnaryExpNode*` - 操作数（递归结构，最终指向 PrimaryExpNode）

---

#### `PrimaryExpNode` - 基本表达式节点
表示基本表达式，如 `(exp)`、`lVal` 或 `number`

**属性：**
- `type: PrimaryExpType` - 类型（`PAREN_EXP`、`LVAL` 或 `NUMBER`）
- `exp: ExpNode*` - 括号表达式（当 type 为 PAREN_EXP 时使用）
- `lVal: LValNode*` - 左值（当 type 为 LVAL 时使用）
- `number: NumberNode*` - 数字（当 type 为 NUMBER 时使用）

---

#### `LValNode` - 左值节点
表示左值（变量或数组元素），如 `a` 或 `arr[0][1]`

**属性：**
- `ident: string` - 标识符名称
- `indices: vector<ExpNode*>` - 数组索引表达式列表（空表示非数组访问）

---

#### `NumberNode` - 数字字面量节点
表示整数常量，如 `123`、`077`、`0xFF`

**属性：**
- `value: int` - 整数值（已解析）

---

#### `FuncCallNode` - 函数调用节点
表示函数调用，如 `func(a, b, c)`

**属性：**
- `ident: string` - 函数名称
- `args: vector<ExpNode*>` - 实参表达式列表

---

#### `RelExpNode` - 关系表达式节点
表示关系运算，如 `a < b`、`c >= d`

**属性：**
- `op: RelOp` - 操作符（`LT`、`GT`、`LE` 或 `GE`）
- `left: RelExpNode*` - 左操作数（可为 nullptr，表示单操作数）
- `right: AddExpNode*` - 右操作数

---

#### `EqExpNode` - 相等表达式节点
表示相等性比较，如 `a == b`、`c != d`

**属性：**
- `op: EqOp` - 操作符（`EQ` 或 `NEQ`）
- `left: EqExpNode*` - 左操作数（可为 nullptr，表示单操作数）
- `right: RelExpNode*` - 右操作数

---

#### `LAndExpNode` - 逻辑与表达式节点
表示逻辑与运算，如 `a && b && c`

**属性：**
- `left: LAndExpNode*` - 左操作数（可为 nullptr，表示单操作数）
- `right: EqExpNode*` - 右操作数

---

#### `LOrExpNode` - 逻辑或表达式节点
表示逻辑或运算，如 `a || b || c`

**属性：**
- `left: LOrExpNode*` - 左操作数（可为 nullptr，表示单操作数）
- `right: LAndExpNode*` - 右操作数

---

#### `CondNode` - 条件表达式节点
条件表达式实际上是 `LOrExpNode`，但为了语义清晰可以单独定义。

**属性：**
- `lOrExp: LOrExpNode*` - 逻辑或表达式

---

#### `ConstExpNode` - 常量表达式节点
常量表达式实际上是 `AddExpNode`，但必须在编译时求值。

**属性：**
- `addExp: AddExpNode*` - 加减表达式

---

## 枚举类型

### `BType` - 基本类型
```cpp
enum class BType {
    INT
};
```

### `FuncType` - 函数类型
```cpp
enum class FuncType {
    VOID,
    INT
};
```

### `BinaryOp` - 二元操作符
```cpp
enum class BinaryOp {
    PLUS,   // +
    MINUS,  // -
    MUL,    // *
    DIV,    // /
    MOD,    // %
    LT,     // <
    GT,     // >
    LE,     // <=
    GE,     // >=
    EQ,     // ==
    NEQ,    // !=
    AND,    // &&
    OR      // ||
};
```

### `UnaryOp` - 一元操作符
```cpp
enum class UnaryOp {
    PLUS,   // +
    MINUS,  // -
    NOT     // !
};
```

### `PrimaryExpType` - 基本表达式类型
```cpp
enum class PrimaryExpType {
    PAREN_EXP,  // (exp)
    LVAL,       // lVal
    NUMBER      // number
};
```

## 设计说明

1. **节点继承关系**：所有节点应继承自一个公共基类 `ASTNode`，提供统一的接口（如 `accept(Visitor*)` 用于访问者模式）。

2. **内存管理**：建议使用智能指针（如 `std::unique_ptr` 或 `std::shared_ptr`）管理节点内存，避免内存泄漏。

3. **表达式优先级**：表达式节点的设计遵循 SysY 语言的运算符优先级：
   - 一元运算符（最高优先级）
   - 乘除模（`*`, `/`, `%`）
   - 加减（`+`, `-`）
   - 关系运算符（`<`, `>`, `<=`, `>=`）
   - 相等运算符（`==`, `!=`）
   - 逻辑与（`&&`）
   - 逻辑或（`||`，最低优先级）

4. **数组处理**：数组维度在声明时使用 `ConstExpNode`（编译时常量），在访问时使用 `ExpNode`（运行时表达式）。

5. **类型系统**：目前 SysY 仅支持 `int` 类型，但设计时应考虑未来扩展（如 `float`、`char` 等）。

