#include <iostream>
#include <plog/Severity.h>
#include <plog/Log.h>
#include <plog/Initializers/RollingFileInitializer.h>
#include <plog/Initializers/ConsoleInitializer.h>

#include "plog/Formatters/TxtFormatter.h"
#include "zcore/incl/args.h"


int main(int argc, char **argv) {
    // TODO: make logging toggleable from command line
    static plog::ConsoleAppender<plog::TxtFormatter> consoleAppender;
    #if 0
        static plog::RollingFileAppender<plog::TxtFormatter> fileAppender("./.zlog/zlog.txt", 100000, 5);
        plog::init(plog::verbose, &consoleAppender).addAppender(&fileAppender);
    #else 
        plog::init(plog::verbose, &consoleAppender);
    #endif     
    PLOGD << "Initialized plog!";

    const auto args = zeal::core::CmdArgs::parse_args(argc, argv);

    
    return 0;
}


