#include <iostream>
#include <plog/Severity.h>
#include <plog/Log.h>
#include <plog/Initializers/RollingFileInitializer.h>
#include <plog/Initializers/ConsoleInitializer.h>

#include "plog/Formatters/TxtFormatter.h"
#include "zast/incl/ast.h"
#include "zbytecode/incl/lib.h"
#include "zcore/incl/lib.h"


#define PROJECT_NAME "zeal"




int main(int argc, char **argv) {
    if(argc != 1) {
        std::cout << argv[0] <<  "takes no arguments.\n";
        return 1;
    }

    // TODO: make logging toggleable from command line
    static plog::ConsoleAppender<plog::TxtFormatter> consoleAppender;
    #if 0
        static plog::RollingFileAppender<plog::TxtFormatter> fileAppender("./.zlog/zlog.txt", 100000, 5);
        plog::init(plog::verbose, &consoleAppender).addAppender(&fileAppender);
    #else 
        plog::init(plog::verbose, &consoleAppender);
    #endif
    
    
    PLOGD << "Initialized plog!";
    
    zeal::core::zcore();
    zeal::bc::bytecode();
    zeal::ast::ast();

    return 0;
}
