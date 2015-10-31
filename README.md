[![Build Status](https://travis-ci.org/hejfelix/Frase.svg?branch=master)](https://travis-ci.org/hejfelix/Frase) [![Codacy Badge](https://api.codacy.com/project/badge/5ac7eafc7a5d4f638d6ce89cdabe318c)](https://www.codacy.com/app/hejfelix/Frase)  [![Join the chat at https://gitter.im/hejfelix/Frase](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/hejfelix/Frase?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

# Frase

Frase is a programming language

Current language features count:

* Basic lambda calculus
* Recursion
* Pattern Matching
* Named terms
* Yet Another Hindlye Milner Type System (YAHMTS)

Technical details:

* Prototype of Interpreter (implemented in Scala, works directly on AST)
* Read Evaluate Print Loop (REPL) with term naming

```haskell
Frase>fib = 0 . 1
added "fib" to context
 
Parsed:       fib = 0 . 1 : Func Int Int
Evaluated:    fib = 0 . 1
time:         5 ms
 
Frase>fib = n . * n (fib (- n 1))
added "fib" to context
 
Parsed:       fib = n . (* n) (fib (- n) (1)) : Func Int Int
Evaluated:    fib = n . (* n) (fib (- n) (1))
time:         5 ms
 
Frase>fib 1
 
Parsed:       fib 1 : Int
Evaluated:    1
time:         4 ms
 
Frase>fib 2
 
Parsed:       fib 2 : Int
Evaluated:    2
time:         1 ms
 
Frase>fib 3
 
Parsed:       fib 3 : Int
Evaluated:    6
time:         3 ms
 
Frase>fib 4
 
Parsed:       fib 4 : Int
Evaluated:    24
time:         3 ms
 
Frase>fib 5
 
Parsed:       fib 5 : Int
Evaluated:    120
time:         3 ms
 
Frase>
```
