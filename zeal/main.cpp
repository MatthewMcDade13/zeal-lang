#include <plog/Initializers/ConsoleInitializer.h>
#include <plog/Initializers/RollingFileInitializer.h>
#include <plog/Log.h>
#include <plog/Severity.h>

#include <csignal>
#include <iostream>

#include "lex.h"
#include "plog/Formatters/TxtFormatter.h"
#include "zcore/incl/args.h"

// std::atomic_bool terminate_prog(false);
volatile bool terminate_prog{false};

void handle_sigint(int signum) {
    if (signum == SIGINT) {
        PLOGD << "Ctrl+C detected. Exiting...";
        terminate_prog = true;
    }
}

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

    std::signal(SIGINT, handle_sigint);

    const auto args = zeal::core::CmdArgs::parse_args(argc, argv);

    std::stringstream ss;
    using std::operator""s;
    for (const auto& f : args.input_args) {
        auto s = std::string(f) + "\0"s;
        ss << s << " | "s;
    }
    const auto fstr = ss.str();

    PLOGI << "CmdArgs: \n\t=> "
          << ((zeal::u32) args.flags & (zeal::u32) flags::CmdArgFlags::Execute)
          << " " << (zeal::u32) flags::CmdArgFlags::Compile << args.input_string
          << "flags len: " << args.input_args.size() << " flags: " << fstr;

    const auto flags = args.flags;

    if (flags::isset_repl(flags)) {
        PLOGI << "flag repl is set!";

    } else if (flags::isset_execute(flags)) {
        PLOGI << "flag execute is set!";
        // if (const auto opt = args.query_evaluate()) {

        // }

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

        while (!terminate_prog) {
            std::cout << "zeal> ";

            if (!std::getline(std::cin, input)) {
                std::cout << "Read EOF, Exiting...\n";
                terminate_prog = true;
            }

            if (input == "exit" || input == "quit") {
                std::cout << "Exiting Zeal Repl\n";
                return 0;
            }

            auto res = zeal::ast::lex::tokenize_memory(input);
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
