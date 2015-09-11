[![Build Status](https://travis-ci.org/hejfelix/Frase.svg?branch=master)](https://travis-ci.org/hejfelix/Frase) [![Codacy Badge](https://api.codacy.com/project/badge/5ac7eafc7a5d4f638d6ce89cdabe318c)](https://www.codacy.com/app/hejfelix/Frase)  [![Join the chat at https://gitter.im/hejfelix/Frase](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/hejfelix/Frase?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

# Frase

Frase is a programming language



```haskell
Frase>double = x . + x x
added "double" to context

Parsed:  double = x . (+ x) (x)
AST: Named(Id(double),Abstr(Id(x),Applic(Applic(Id(+),Id(x)),Id(x))))
Evaluated: double = x . (+ x) (x)

Frase>myValue = 21
added "myValue" to context

Parsed:  myValue = 21
AST: Named(Id(myValue),Integer(21))
Evaluated: myValue = 21

Frase>double myValue

Parsed:  double myValue
AST: Applic(Id(double),Id(myValue))
Evaluated: 42

Frase>true yes no

Parsed:  ((true) (yes)) (no)
AST: Applic(Applic(Bit(true),Id(yes)),Id(no))
Evaluated: yes

Frase>false yes no

Parsed:  ((false) (yes)) (no)
AST: Applic(Applic(Bit(false),Id(yes)),Id(no))
Evaluated: no

Frase>
```


```haskell
Frase>fac = n . (<= n 1) (1) (* (n) (fac (- n 1)))
added "fac" to context

Parsed:  fac = n . (((<= n) (1)) (1)) ((* n) (fac (- n) (1)))
AST: Named(Id(fac),Abstr(Id(n),Applic(Applic(Applic(Applic(Id(<=),Id(n)),Integer(1)),Integer(1)),Applic(Applic(Id(*),Id(n)),Applic(Id(fac),Applic(Applic(Id(-),Id(n)),Integer(1)))))))
Evaluated: fac = n . (((<= n) (1)) (1)) ((* n) (fac (- n) (1)))

Frase>fac 0

Parsed:  fac 0
AST: Applic(Id(fac),Integer(0))
Evaluated: 1

Frase>fac 3

Parsed:  fac 3
AST: Applic(Id(fac),Integer(3))
Evaluated: 6

Frase>fac 4

Parsed:  fac 4
AST: Applic(Id(fac),Integer(4))
Evaluated: 24

Frase>fac 5

Parsed:  fac 5
AST: Applic(Id(fac),Integer(5))
Evaluated: 120

Frase>
```
