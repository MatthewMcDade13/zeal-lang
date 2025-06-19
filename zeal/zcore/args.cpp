#include "args.h"


zl_CommandArgs zl_parse_args(int argc, char** argv) ZEAL_NOEXCEPT {
  return {};
}

// #include <algorithm>
// #include <cstring>
// #include <ios>
// #include <sstream>
// #include <tuple>
// #include "plog/Log.h"

// using namespace zeal;

// static Tup2<String, Vec<Str>> collect_args(int argc, char** argv);
// static void parse_flag(const std::string_view flag,
//                        core::flags::CmdArgFlags& outflags);

// namespace zeal::core {

// CmdArgs CmdArgs::parse_args(int argc, char** argv) {
//     auto [argline, args] = collect_args(argc, argv);
//     CompileType comptype = CompileType::None;
//     flags::CmdArgFlags flags = flags::CmdArgFlags::None;
//     ArgParseError err{};

//     for (const auto arg : args) {
//         if (arg.contains('-')) {
//             parse_flag(arg, flags);
//         }
//     }

//     // TODO: This need more love, task for later! lol
//     // gunna set defaults for now...

//     comptype = CompileType::ZvmInterp;

//     return CmdArgs(comptype, flags, err, std::move(args), std::move(argline));
// }

// Opt<Str> CmdArgs::query_outdir() const {
//     const auto len = this->input_args.size();
//     for (usize i = 0; i < len; i++) {
//        const auto item = this->input_args[i]; 
//        if (item.contains("-o")) {
//            // make sure this flag wasnt the last argument
//            if (i + 1 >= len) {
//                // we got the flag but no argument was provided
//                return {};
//            }
//            return this->input_args[i + 1];
//        }
//     }
//     return {};
// }

// Opt<Str> CmdArgs::query_evaluate() const {

    
//     const auto len = this->input_args.size();
//     for (usize i = 0; i < len; i++) {
//        const auto item = this->input_args[i]; 
//        if (item.contains("-e")) {
//            // make sure this flag wasnt the last argument
//            if (i + 1 >= len) {
//                // we got the flag but no argument was provided
//                return {};
//            }
//            return this->input_args[i + 1];
//        }
//     }
//     return {};    
// }

// }  // namespace zeal::core


// static void parse_flag(const std::string_view flag,
//                        core::flags::CmdArgFlags& outflags) {
    
//     using namespace core::flags;
//     // used for flags that need additional flags
//     for (usize i = 0; i < flag.size(); i++) {
//         const auto item = flag[i];
//         PLOGD << "parsing flag: " << item;
//         if (item == '-') {
//             continue;
//         }
//         switch (item) {
//             case 'C':  // fallthrough
//             case 'c': {
//                 outflags |= CmdArgFlags::Compile;
//             } break;
//             case 'E':  // fallthrough
//             case 'e': {
//                 PLOGD << "Matched flag e!";
//                 PLOGD << "outflags before: " << (u32)outflags;

//                 outflags = outflags | CmdArgFlags::Execute;

//                 PLOGD << "outflags after: " << (u32)outflags;
//             } break;
//             case 'S':  // fallthrough
//             case 's': {
//                 outflags |= CmdArgFlags::CompileSrcString;
//             } break;
//             case 'M':  // fallthrough
//             case 'm': {
//                 outflags |= CmdArgFlags::CompileModule;
//             } break;
//             case 'R':  // fallthrough
//             case 'r': {
//                 outflags |= CmdArgFlags::CompileRoot;
//             } break;
//             default: {
//                 return;
//             } break;
//         }
//     }
// }

// static Tup2<String, Vec<Str>> collect_args(int argc, char** argv) {
//     // first is always executable name, we dont care about that, so
//     // assume no args
//     if (argc <= 1) {
//         return {};
//     }
//     std::stringstream ss;
//     /// index of start of string and string length
//     Vec<Tup2<usize, usize>> ranges;
//     ranges.reserve(argc);

//     // Build a String and note the indexes and lengths of each string
//     // so we can make a list of string_views into resulting string later
//     for (int i = 0; i < argc; i++) {
//         const char* arg = argv[i];
//         const auto len = std::strlen(arg);
//         const auto begin = ss.view().size();
//         ranges.emplace_back(std::make_tuple(begin, len));

//         ss << arg << " ";
//     }
//     // stop character (and newline)
//     ss << "$\n";
//     String src = ss.str();

//     Vec<Str> args{};
//     args.reserve(argc);

//     for (const auto& item : ranges) {
//         const auto index = std::get<0>(item);
//         const auto len = std::get<1>(item);

//         const auto begin = &src[index];
//         const auto view = Str(begin, len);

//         args.emplace_back(view);
//     }

//     return std::make_tuple(src, args);
//     // return std::make_tuple(std::move(src), std::move(args));
// }
