# zeal-lang
Zeal Programming Language.

#### Goals
- Syntax is lua-ish in ml style 
- Inferred static types, dynamic types are possible, but compiler and runtime try to avoid it.
- JIT or AOT compilation 
- Module System
- documentation generation and typespecs similar to RustDoc or Elixir's @doc 
- Runtime AST and compile time macros
- package manager & cli tool for Zeal (simliar to npm/cargo, name tentative)
- Structs, no classes
- Traits: Something between Rust and Go, or at least Behaviors like Elixir
- Pattern matching / Destructuring (thats gunna be a while before this is impled lol)
- Optional Types (nil exists but I may remove it in favor of optional types, nonetheless Optional types will be a feature no matter what)
- Pipe operator
- Pass instance to method function automatically (instance.method() and method(instance) are both legal syntax and express the same method/function call)


#### Open Considerations
- No Nil and Optional Types? or both?
- Mutiple Dispatch or Static/Dyn Dispatch

#### Declarations
- var :: binding can be reassigned, eg: var x = 1; x = 3; Does not allow rebinding (shadowing). 
- let :: similar to Rust's let, allows rebinding (shadowing), but not reassignment
- const :: Does not allow rebinding, or reassignment, unique identifier in current scope. 

#### Compile Targets
- ECMAScript/WebAssembly
- LLVM:
- ZealVM



#### Tentative syntax

`
mod math

fn add(a i32, b i32): i32 
    a + b
end

fn sub(a, b) = a - b

struct Point 
  x u32
  y u32
end

fn mutate_point(p mut Point) 
  p.x = p.x * p.x
end

`


## Installing
- Because of ABI weirdness, its best if you compile Zeal binaries from scratch with clang/LLVM.

#### Install req
- clang++ >= 19
- llvm >= 19
- meson >= 1.6.1

```bash
  make setup
  make build
  
```
