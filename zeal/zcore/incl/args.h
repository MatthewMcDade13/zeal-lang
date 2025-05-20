#pragma once
#include <type_traits>

#include "common.h"

namespace zeal::core {

namespace flags {

/// Cli command argument flags
enum class CmdArgFlags : u32 {
    /// No arguments, run default (now default is version, but very soon will be
    /// repl...)
    None = 0,
    /// Execute Input, if this flag is set, zeal will try execute any inputs that
    /// were loaded or read from cli, if none it will try to run a .zl file in the
    /// current working
    /// directory, or will run the project if in a directory with a zl.json file ( or
    /// in directories specified in zl.json or through cli with [-p --project] flags)
    /// and treat that directory as root path
    ///
    /// This is ignored if 'repl' is any argument passed to zeal
    ///
    ///  ```bash
    ///  zeal exec "path/to/main.zl"
    ///  ```
    ///
    ///  -(E)xecute (s)ource_string
    ///  ```bash
    ///  zeal -Es "let x = 50; print x;"
    ///  ```
    ///
    ///
    Execute = 1 << 1,
    /// zeal repl
    Repl = 1 << 2,
    /// Source string to be executed, or compiled if used with [-c --compile] Flag
    /// will be ignored if 'repl' is any arugment passed to zeal.
    /// ```bash
    /// zeal [-s --source] "let x = 40; print x;"
    /// ```
    SrcString = 1 << 3,
    /// (C)ompile
    // zeal -C "path/to/some/file.zl"
    Compile = 1 << 4,
    /// (C)ompile (s)ource_string
    /// zeal -Cs "let x = 50; print x;"
    CompileSrcString = 1 << 5,
    ///
    /// (C)ompile (r)oot
    /// zeal -Cr .
    CompileRoot = 1 << 6,
    /// (C)ompile (m)odule
    /// zeal -Cm "path/to/your_module.zl"
    CompileModule = 1 << 7,

    /// (O)utput directory
    /// can also be -o or --outdir or --out
    /// zeal -C "path/to/file.zl" -O "./ouput/directory"
    OutDir = 1 << 8,
    /// (i)nitializes a new Zeal project in directory specified
    /// zeal -i .
    /// zeal --init "path/to/proj/root"
    InitProject = 1 << 9,

};

constexpr CmdArgFlags operator|(CmdArgFlags lhs, CmdArgFlags rhs) noexcept {
    const auto l = static_cast<std::underlying_type_t<CmdArgFlags>>(lhs);

    const auto r = static_cast<std::underlying_type_t<CmdArgFlags>>(rhs);

    return static_cast<CmdArgFlags>(l | r);
}
constexpr CmdArgFlags& operator|=(CmdArgFlags& lhs, CmdArgFlags rhs) {
    lhs = lhs | rhs;
    return lhs;
}

namespace priv_impl {

template <typename T, T Mask>
constexpr bool _isset_(const auto flag) noexcept {
    constexpr auto FLAG = static_cast<u32>(Mask);
    const auto fl = static_cast<u32>(flag);
    return (FLAG & fl) == FLAG;
}

}  // namespace priv_impl

#ifndef isset
#define isset(FLAG, TYPE, VARIANT) priv_impl::_isset_<TYPE, TYPE::VARIANT>(FLAG)
#endif

#ifndef isset_args
#define isset_args(FL, V) isset(FL, CmdArgFlags, V)
#endif

constexpr bool isset_repl(const CmdArgFlags fl) noexcept {
    return isset_args(fl, Repl);
}

constexpr bool isset_execute(const CmdArgFlags fl) noexcept {
    return isset_args(fl, Execute);
}

constexpr bool isset_compile_src_string(const CmdArgFlags fl) noexcept {
    return isset_args(fl, CompileSrcString);
}

constexpr bool isset_compile_root(const CmdArgFlags fl) noexcept {
    return isset_args(fl, CompileRoot);
}

constexpr bool isset_compile_module(const CmdArgFlags fl) noexcept {
    return isset_args(fl, CompileModule);
}

constexpr bool isset_src_string(const CmdArgFlags fl) noexcept {
    return isset_args(fl, SrcString);
}

constexpr bool isset_compile(const CmdArgFlags fl) noexcept {
    return isset_args(fl, Compile);
}


constexpr bool isset_outdir(const CmdArgFlags fl) noexcept {
    return isset_args(fl, OutDir);
}

}  // namespace flags

/// The type of output we will tell our backed to target.
/// This does not specifiy the actual physical architecture of
/// the target machine, as this is handled by each individual CompileType
enum class CompileType {
    /// Fully Interpreted straight from AST
    None,
    /// Lex, parse, compile to Zvm bytecode, interpret bytecode
    ZvmInterp,
    /// lex, parse, compile to zvm bytecode on the fly, then jit (ill have to
    /// implement this myself to keep binaries small and portable,
    /// so ill have to look into what i can reasonly do here, maybe an ARM64 JIT
    /// compiler???? hmmmmm)
    ZvmJit,
    /// lex, parse, compile to Zvm bytecode and save to a file (does not execute
    /// anything)
    Zvm,
    /// lex, parse, compile to LLVM IR and save to a file (does not execute anything)
    LLVM,
    /// lex, parse, compile to LLVM, then jit
    LLVMJit,
};

struct ArgParseError {
    enum : u8 {
        None = 0,
        InvalidArg,
        FileNoteFound,
        DirectoryNotFound,
    } ty{0};
    /// Index of the arg(s) that caused error
    usize arg_index{0};
};

using ArgParseError_t = decltype(ArgParseError::ty);

struct CmdArgs {
    CompileType compile_type;
    flags::CmdArgFlags flags{flags::CmdArgFlags::None};

    ArgParseError error;

    /// all arguements passed from cli, does not include 'zeal'
    /// (input_string[0] != 'zeal')
    Vec<Str> input_args{};
    String input_string{};

    /// NO COPY NOOOOOOOOO !!!!!!!!!!!
    CmdArgs(const CmdArgs& other) = delete;
    /// NO COPY NOOOOOOOOOOO !!!!!!!
    CmdArgs& operator=(const CmdArgs& others) = delete;

    constexpr CmdArgs() noexcept
        : compile_type(CompileType::None),
          flags(flags::CmdArgFlags::None),
          error(),
          input_string() {}

    constexpr CmdArgs(CompileType comptype, flags::CmdArgFlags flags_in,
                      ArgParseError errin, Vec<Str>&& args, String&& input) noexcept
        : compile_type(comptype),
          flags(flags_in),
          error(errin),
          input_args(std::move(args)),
          input_string(std::move(input)) {}

    constexpr CmdArgs(CompileType comptype, flags::CmdArgFlags flags_in,
                      Vec<Str>&& input_args_in, String&& input) noexcept
        : compile_type(comptype),
          flags(flags_in),
          input_args(std::move(input_args_in)),
          input_string(std::move(input)) {}

    /// checks if this instance was default constructed
    constexpr bool is_empty() const noexcept {
        return this->compile_type == CompileType::None &&
               static_cast<u32>(this->flags) == 0;
    }

    static CmdArgs parse_args(int argc, char** argv);
    /// Tries to get the value for outdir, returns nullopt if unsuccessful or
    /// value is nullopt or not present
    Opt<Str> query_outdir() const;
    /// Tries to get the argument passed to execute flag
    /// or nullopt if not present 
    Opt<Str> query_evaluate() const;
};

}  // namespace zeal::core
