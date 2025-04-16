# _Y_ lang

They _Y_ programming language. 

It's pronounced "why?!!", because people kept asking Why would
I write a native compiler. Why did you do it in Haskell initially?
Why was it originally called yacll? Just Why?

## Why?

That's a question I keep asking myself every day, also "How?"

## The _Y_ Foundation

This language backed up by the Y foundation where I'm the director, founder, engineer, secretary
and decision board.

# Passes

The compiler goes through a couple of passes and intermediate representations

1. Atomizer - Removes all the complex expressions and leave them in an atomic shape.
It is from Ast to Ast
2. StmtsToX86 - Pick the stmts from the AST and convert it to a type-safe NASM intermediate
representation. It goes from Ast to Nasm
3. X86ToTextProg - Picks the NASM IR and transforms it to a final text, used to generate an .asm 
later on
4. TypeChecker - Aggressive type checker. Aggressive as in, it will insult you and misstreat you 
if it finds an incorrect program.

# How to compile a .y file

```
yc -o output_binary_file_name -i program.y
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


# Language TLDR

## Mutability 

By default variables are immutable. The mutability of a variable is defined in the type 
so operations that mutate variables are type checked

e.g. 

```
// Immutable type
x : u64 = 3;

// Mutable type
i : mut u64 = 0;

// Automatic Mutability cast
x : mut u64 = x;
```

