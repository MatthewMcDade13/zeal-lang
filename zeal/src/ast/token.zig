const std = @import("std");

pub fn hello() void {
    std.debug.print("Hello from {s}", .{"token.zig"});
}

pub const Tok = struct {
    lexeme: *const []u8,
    literal: ?Literal,

    lineno: usize,
    linecol: usize,

    toktype: TokType,
};

pub const Lexeme = struct {
    pub const OpenParen = '(';
    pub const CloseParen = ')';
    pub const OpenBracket = '[';
    pub const CloseBracket = ']';
    /// Open Curly brace '{'
    pub const OpenBrace = '{';
    /// Close Curly brace '}'
    pub const CloseBrace = '}';
    pub const Colon = ':';
    pub const SemiColon = ';';
    pub const SingleQuote = '\'';
    pub const DoubleQuote = '"';

    pub const Equals = '=';

    pub const Gt = '>';
    pub const Lt = '<';
    pub const Bar = '|';
    pub const DoubleBar = "||";
    pub const DoubleGt = ">>";
    pub const DoubleLt = "<<";

    pub const StarEq = "*=";
    pub const Star = '*';
    pub const ForwardSlashEq = "/=";
    pub const ForwardSlash = '/';
    pub const BackSlash = '\\';
    pub const Minus = '-';
    pub const MinusEq = "-=";
    pub const PlusEq = "+=";
    pub const Plus = '+';
    pub const Comma = ',';
    pub const Period = '.';

    pub const BangEq = "!=";
    pub const DoubleEq = "==";
    pub const DoublePlus = "++";
    pub const DoubleMinus = "--";
    pub const DoubleStar = "**";

    pub const FatArrow = "=>";
    pub const ArrowRight = "->";
    pub const ArrowLeft = "<-";

    pub const Gte = ">=";
    pub const Lte = "<=";
    pub const Bang = '!';
    pub const QMark = '?';
    pub const Newline = '\n';
    pub const Space = ' ';

    pub const Begin = "begin";
    pub const Do = "do";
    pub const End = "end";

    pub const Pub = "pub";

    pub const Let = "let";
    pub const In = "in";
    pub const Mut = "mut";
    pub const Const = "const";

    pub const When = "when";
    pub const If = "if";
    pub const Else = "else";
    pub const Elseif = "elseif";
    pub const While = "while";
    pub const For = "for";
    pub const Foreach = "foreach";
    pub const OrElse = "orelse";

    pub const Enum = "enum";
    pub const Module = "module";
    pub const NewType = "newtype";
    pub const Func = "func";
    pub const Struct = "struct";
    pub const Alias = "alias";
};

pub const TokType = enum(u8) {
    /// Any named identifier that is not a keyword or prefixed by ':'
    Symbol,
    /// Any keyword or identifier prefixed with ':' All Runes are unique, such that
    /// :name == :name
    Rune,
    /// Any numeric, boolean, string, or struct/table literal
    Literal,
};

pub const Literal = union(enum) {
    num: i64,
    float: f64,
    bool: bool,
    /// This will include quotes in string if the TokType associated with this Literal is TokType.Literal
    /// otherwise will be string not including quotes
    ///
    /// Strings pointed to must be part of the AST and/or Bytecode Rune/Symbol Table
    ///     OR
    /// Must outlive the AST/Bytecode Rune/Symbol Table
    string: *const []u8,

    /// Unique Keywords and user defined Runes (prefixed with ':' of the form :<name> or :"<name>")
    rune: *const []u8,

    /// A keyword/symbol that is of length 1. ex: '+'
    key_char: u8,
};

pub fn tokenize(file: *const []u8) !*const [*][]u8 {
    const contents = try std.fs.cwd().openFile(file);
    const toks = std.mem.tokenizeAny(u8, contents.readToEndAlloc(std.heap.ArenaAllocator.allocator(), 10000000), " \n");
    while (toks.next()) |t| {
        std.debug.print("{s}", t);
    }
}
