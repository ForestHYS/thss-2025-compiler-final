#pragma once

#include <string>
#include <vector>
#include <memory>

namespace sysy {

// 前向声明
class ASTVisitor;

/**
 * AST节点基类
 * 所有AST节点都继承自此类
 */
class ASTNode {
public:
    virtual ~ASTNode() = default;
    
    /**
     * 接受访问者模式
     * @param visitor 访问者对象
     */
    virtual void accept(ASTVisitor* visitor) = 0;
    
    /**
     * 获取节点类型名称（用于调试）
     */
    virtual std::string getNodeType() const = 0;
};

} // namespace sysy

