#include <plog/Initializers/RollingFileInitializer.h>
#include <plog/Initializers/ConsoleInitializer.h>
#include <plog/Log.h>
#include <plog/Severity.h>

#include <csignal>
#include <iostream>

#include "common.h"
#include "lex.h"
#include "plog/Formatters/TxtFormatter.h"
#include "zcore/incl/args.h"

zeal::IOResult run_cli(int argc, char** argv);

int main(int argc, char** argv) {
    using namespace zeal::core;

    // TODO: make logging toggleable from command line
    static plog::ColorConsoleAppender<plog::TxtFormatter> consoleAppender;
#if 0
        static plog::RollingFileAppender<plog::TxtFormatter> fileAppender("./.zlog/zlog.txt", 100000, 5);
        plog::init(plog::verbose, &consoleAppender).addAppender(&fileAppender);
#else
    plog::init(plog::verbose, &consoleAppender);
#endif
    PLOGD << "Initialized plog!";

    if (const auto res = run_cli(argc, argv); !res.has_value()) {
        PLOGF
            << "================== Unrecoverable Error occured! ==================";
        PLOGF << res.error().to_string();
        return 1;
    }

    PLOGD << "Zeal was run from the command line and exited normally...";
    return 0;

}

zeal::IOResult run_cli(int argc, char** argv) {
    using namespace zeal::core;
    const auto args = zeal::core::CmdArgs::parse_args(argc, argv);

    const auto fls = args.flags;

    if (flags::isset_repl(fls)) {
        PLOGI << "flag repl is set!";

    } else if (flags::isset_execute(fls)) {
        PLOGI << "flag execute is set!";

        PLOGF << "Execute/eval flag was set but not argument was provided!";
        return {};

    } else if (flags::isset_outdir(fls)) {
        PLOGI << "outdir flag is set!";
        return {};

    } 
        std::cout << "Zeal Programming Language v0.1 (GPLv3) \n"
                  << "Matthew McDade :: https://github.com/MatthewMcDade13\n\n\n\n";

        zeal::String input;

        for (;;) {
            std::cout << "zeal> ";

            if (!std::getline(std::cin, input)) {
                std::cout << "Read EOF, Exiting...\n";
                return {};
            }

            if (input == "exit" || input == "quit") {
                std::cout << "Exiting Zeal Repl\n";
                return {};
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
                PLOGF << "Failed to tokenize input: '" << input
                      << "': " << res.error().message;
                continue;
            }
        }

        return {};
}
