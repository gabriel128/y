# _Y_ lang

They _Y_ programming language. 

It's pronounced "why?!!", because people kept asking Why would
I write a native compiler. Why did you do it in Haskell initially?
Why was it originally called yacll? Just Why?

## Why?

That's a question I keep asking myself every day, also "How?"

## The _Y_ Foundation

This language backed up by the Y foundation where I'm the director, founder, engineer, secretary
and main board member. 

# Passes

The compiler goes through a couple of passes and intermediate representations. 

0. Parser - Parses a potential _Y_ program into the AST which is Maybe typed, a `Nothing` type means that 
   the has to be inferred later on.
1. TypeChecker - Aggressive type checker. Aggressive as in: it will insult you and misstreat you 
   if it finds an incorrect program, this will add the needed frustration for this language to become
   popular, like rust. It will transform a maybe typed program into a typed program.
   The Haskell type checker ensures at compile time that we can't construct a program with uninferred types.
   In other words, we have a compile time checked type checker.
2. Atomizer - Removes all the complex expressions and leaves them in an atomic shape _a la_ TAC 
   (be careful celiacs using _Y_).
   It converts the Ast.Expr expressions into AtomIr.AExpr, ensuring at compile time that the expressions
   are in TAC form.
3. StmtsToX86 - Pick the stmts from the TAC and converts them to a type-safe NASM intermediate
   representation. 
4. X86ToTextProg - Picks the NASM IR and transforms it to a final text, used to generate an .asm 
   later on

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

