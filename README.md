# YACLL 

Yet another C like language

# Passes

The compiler goes through a couple of passes and intermediate representations

1. AtomizeAst - Removes all the complex expressions and leave them in an atomic shape.
It is from Ast to Ast
2. StmtsToX86 - Pick the stmts from the AST and convert it to a type-safe NASM intermediate
representation. It goes from Ast to Nasm
3. X86ToTextProg - Picks the NASM IR and transforms it to a final text, used to generate an .asm 
later on

# How to compile a .yacll file

```
yacll -o output_binary_file_name -i program.yacll
```

# Effects

This project represents effects using fused-effects.

# Prog Examples

check the /examples folder.
