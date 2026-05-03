# .plan

## Interpreter and toolchain

* Semantic analysis
  * ~~Referencing a variable that doesn't exist is a runtime error~~
  * Optimise environment and variable access
* cliqfmt
* Neovim plugin
* Extensive test suite
  * Required before building a compiler
* Bytecode VM

## REPL

* Multi-line support
* Inspect currently defined vars
* View functions in scope
* Reference previous expression results

## Syntax

* Can we have optional parameters for functions?
* Array slices
* Quoted-literal arrays
* Non-decimal number expressions
* Sets?
* IO functions
  * spit
  * apppend
  * slurp
  * system (and some syntax like Perl's backticks)
* cond/when expressions?
* Bitwise operators
* Regex support
