#include <iostream>
#include "Compiler.h"

int main(int argc, const char *argv[]) {
  if (argc < 3) {
    std::cerr << "Usage: ./compiler <input-file> <output-file>"
              << std::endl;
    return 1;
  }
  
  sysy::Compiler compiler;
  if (compiler.compile(argv[1], argv[2])) {
    return 0;
  } else {
    return 1;
  }
}