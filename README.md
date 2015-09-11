[![Build Status](https://travis-ci.org/hejfelix/Frase.svg?branch=master)](https://travis-ci.org/hejfelix/Frase) [![Codacy Badge](https://api.codacy.com/project/badge/5ac7eafc7a5d4f638d6ce89cdabe318c)](https://www.codacy.com/app/hejfelix/Frase)  [![Join the chat at https://gitter.im/hejfelix/Frase](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/hejfelix/Frase?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

# Frase

Frase is a programming language

```haskell
Frase>map = f . Nil . Nil
added "map" to context

Parsed:       map = f .  .  : b -> Nil
Evaluated:    map = f .  . 
time:         3 ms

Frase>map = f . (Cons x xs) . Cons (f x) (map f xs)
added "map" to context

Parsed:       map = f . [x,xs] . [f x,(map f) (xs)] : b -> Cons
Evaluated:    map = f . [x,xs] . [f x]
time:         11 ms

Frase>map (+ 1) ([1,2,3,4])

Parsed:       (map + 1) ([1,2,3,4]) : a
Evaluated:    [2,3,4,5]
time:         9 ms

Frase>
```
