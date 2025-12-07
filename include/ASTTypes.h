#pragma once

namespace sysy {

/**
 * 基本类型枚举
 */
enum class BType {
    INT
};

/**
 * 函数返回类型枚举
 */
enum class FuncType {
    VOID,
    INT
};

/**
 * 二元操作符枚举
 */
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

/**
 * 一元操作符枚举
 */
enum class UnaryOp {
    PLUS,   // +
    MINUS,  // -
    NOT     // !
};

/**
 * 基本表达式类型枚举
 */
enum class PrimaryExpType {
    PAREN_EXP,  // (exp)
    LVAL,       // lVal
    NUMBER      // number
};

} // namespace sysy

