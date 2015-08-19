[![Build Status](https://travis-ci.org/hejfelix/Frase.svg?branch=master)](https://travis-ci.org/hejfelix/Frase)

# Frase
Frase is a programming language

```scala
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
