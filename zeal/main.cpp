#include <iostream>
#include "zbytecode/incl/lib.h"
#include "zcore/incl/lib.h"
#include "zvm/incl/lib.h"

#define PROJECT_NAME "zeal"

int main(int argc, char **argv) {
    if(argc != 1) {
        std::cout << argv[0] <<  "takes no arguments.\n";
        return 1;
    }
    std::cout << "This is project  AYYYYLMEOOOOO " << PROJECT_NAME << ".\n";
    zeal::core::zcore();
    zeal::bc::bytecode();
    zeal::zvm::zvm();

    return 0;
}
