# zeal-lang
Zeal Programming Language.

#### Goals
- Syntax is lua-ish in ml style 
- Native embedable Rust bindings.
- Unobstructive Static Typing (somewhat similar to Go) 
- JIT or AOT compilation 
- Module System
- documentation generation and typespecs similar to RustDoc or Elixir's @doc 
- Runtime AST and compile time macros
- package manager & cli tool for Zeal (simliar to npm/cargo, name tentative)
- Structs, no classes
- Traits: Something between Rust and Go 
- Pattern matching / Destructuring

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
- ECMAScrit/WebAssembly
- LLVM:
- [Mu Micro Virtual Machine](https://microvm.github.io/)  
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

