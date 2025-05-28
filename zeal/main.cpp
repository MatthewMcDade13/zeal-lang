#include <plog/Initializers/ConsoleInitializer.h>
#include <plog/Initializers/RollingFileInitializer.h>
#include <plog/Log.h>
#include <plog/Severity.h>

#include <csignal>
#include <iostream>

#include "lex.h"
#include "plog/Formatters/TxtFormatter.h"
#include "zcore/incl/args.h"


int main(int argc, char** argv) {
    using namespace zeal::core;

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

    const auto flags = args.flags;

    if (flags::isset_repl(flags)) {
        PLOGI << "flag repl is set!";

    } else if (flags::isset_execute(flags)) {
        PLOGI << "flag execute is set!";

        PLOGF << "Execute/eval flag was set but not argument was provided!";
        return 0;

    } else if (flags::isset_outdir(flags)) {
        PLOGI << "outdir flag is set!";
        return 0;

    } else {
        PLOGW << "No supported flags passed to zeal!\n\t=> Defaulting to Repl "
                 "exiting";

        std::cout << "Zeal Programming Language v0.1 (GPLv3) \n"
                  << "Matthew McDade :: https://github.com/MatthewMcDade13\n\n\n\n";

        zeal::String input;

        for(;;) {
            std::cout << "zeal> ";

            if (!std::getline(std::cin, input)) {
                std::cout << "Read EOF, Exiting...\n";
                return 0;
            }

            if (input == "exit" || input == "quit") {
                std::cout << "Exiting Zeal Repl\n";
                return 0;
            }

            auto res = zeal::ast::lex::tokenize_input(input);
            if (res.has_value()) {
                PLOGD << "Lex Success!!";
                const auto toks = res.value(); 
                for (const auto& t : toks) {
                    PLOGD << t.to_string();
                }
                continue;
            } else {
                PLOGF << "Failed to tokenize input: '" << input << "': " << res.error().message;
                continue;
            }
        }

        return 0;
    }
}
