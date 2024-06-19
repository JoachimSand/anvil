# Anvil
A WIP compiler for the anvil language.



## Assembling generated LLVM IR

```
llc -filetype=asm test.bc
gcc test.s -o main 
```
