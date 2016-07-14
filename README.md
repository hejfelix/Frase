[![Build Status](https://travis-ci.org/hejfelix/Frase.svg?branch=master)](https://travis-ci.org/hejfelix/Frase) 
[![Codacy Badge](https://api.codacy.com/project/badge/5ac7eafc7a5d4f638d6ce89cdabe318c)](https://www.codacy.com/app/hejfelix/Frase)  [![Codacy Badge](https://api.codacy.com/project/badge/Coverage/5ac7eafc7a5d4f638d6ce89cdabe318c)](https://www.codacy.com/app/hejfelix/Frase?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=hejfelix/Frase&amp;utm_campaign=Badge_Coverage)
[![Join the chat at https://gitter.im/hejfelix/Frase](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/hejfelix/Frase?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

# Frase

Frase is a programming language

Current language features count:

* Basic lambda calculus
* Recursion
* Pattern Matching
* Named terms
* Yet Another Hindlye Milner Type System (YAHMTS)
* (Very) Early stages of Algebraic Data Types

Technical details:

* Prototype of Interpreter (implemented in Scala, works directly on AST)
* Read Evaluate Print Loop (REPL) with term naming

```haskell
Frase>fac = 0 . 1
added "fac" to context
 
Parsed:       fac = 0 . 1 : Func Int Int
Evaluated:    fac = 0 . 1
time:         5 ms
 
Frase>fac = n . * n (fac (- n 1))
added "fac" to context
 
Parsed:       fac = n . (* n) (fac (- n) (1)) : Func Int Int
Evaluated:    fac = n . (* n) (fac (- n) (1))
time:         5 ms
 
Frase>fac 1
 
Parsed:       fac 1 : Int
Evaluated:    1
time:         4 ms
 
Frase>fac 2
 
Parsed:       fac 2 : Int
Evaluated:    2
time:         1 ms
 
Frase>fac 3
 
Parsed:       fac 3 : Int
Evaluated:    6
time:         3 ms
 
Frase>fac 4
 
Parsed:       fac 4 : Int
Evaluated:    24
time:         3 ms
 
Frase>fac 5
 
Parsed:       fac 5 : Int
Evaluated:    120
time:         3 ms
 
Frase>
```
