# _Y_ lang

They _Y_ programming language. 

It's pronounced "why?!!", because people kept asking Why would
I write a native compiler. Why did you do it in Haskell initially?
Why was it originally called yacll? Just Why?

## Why?

That's a question I keep asking myself every day, also "How?"

# Passes

The compiler goes through a couple of passes and intermediate representations

1. Atomizer - Removes all the complex expressions and leave them in an atomic shape.
It is from Ast to Ast
2. StmtsToX86 - Pick the stmts from the AST and convert it to a type-safe NASM intermediate
representation. It goes from Ast to Nasm
3. X86ToTextProg - Picks the NASM IR and transforms it to a final text, used to generate an .asm 
later on
4. TypeChecker - WIP

# How to compile a .y file

```
y -o output_binary_file_name -i program.y
```

# Effects

This project represents effects using fused-effects.

# Prog Examples

check the /examples folder.

# Build and run docker for X86_64

Build

``` sh
make docker-build-86_64_cache
make docker-build-x86_64
```

Run

``` sh
make docker-x86_64
```

# Tests

They are meant to run in the docker container or a linux machine

```
make test
```

