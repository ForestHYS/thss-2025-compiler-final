
// Generated from SysYLexer.g4 by ANTLR 4.13.1

#pragma once


#include "antlr4-runtime.h"




class  SysYLexer : public antlr4::Lexer {
public:
  enum {
    CONST = 1, INT = 2, VOID = 3, IF = 4, ELSE = 5, WHILE = 6, BREAK = 7, 
    CONTINUE = 8, RETURN = 9, PLUS = 10, MINUS = 11, MUL = 12, DIV = 13, 
    MOD = 14, ASSIGN = 15, EQ = 16, NEQ = 17, LT = 18, GT = 19, LE = 20, 
    GE = 21, NOT = 22, AND = 23, OR = 24, L_PAREN = 25, R_PAREN = 26, L_BRACE = 27, 
    R_BRACE = 28, L_BRACKT = 29, R_BRACKT = 30, COMMA = 31, SEMICOLON = 32, 
    IDENT = 33, INTEGER_CONST = 34, WS = 35, LINE_COMMENT = 36, MULTILINE_COMMENT = 37
  };

  explicit SysYLexer(antlr4::CharStream *input);

  ~SysYLexer() override;


  std::string getGrammarFileName() const override;

  const std::vector<std::string>& getRuleNames() const override;

  const std::vector<std::string>& getChannelNames() const override;

  const std::vector<std::string>& getModeNames() const override;

  const antlr4::dfa::Vocabulary& getVocabulary() const override;

  antlr4::atn::SerializedATNView getSerializedATN() const override;

  const antlr4::atn::ATN& getATN() const override;

  // By default the static state used to implement the lexer is lazily initialized during the first
  // call to the constructor. You can call this function if you wish to initialize the static state
  // ahead of time.
  static void initialize();

private:

  // Individual action functions triggered by action() above.

  // Individual semantic predicate functions triggered by sempred() above.

};

