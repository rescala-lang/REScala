---
title: Vars
version: 0.3
number: 2.1
permalink: /manual/Vars
---
## Vars

### Defining Vars
Programmers express reactive
computations starting from vars. Vars wrap normal Scala values. For
example, \code{Var(2)} creates a var with an \code[Int] value and
initializes the var to the value 2. Vars are parametric types. A var
that carries integer values has type \code{Var[Int]}. The following
code snippet shows valid var declarations.

```scala
val a = Var(0)
val b = Var("Hello World")
val c = Var(false)
val d: Var[Int] = Var(30)
val e: Var[String] = Var("REScala")
val f: Var[Boolean] = Var(false)
```

### Assigning Vars
Vars can be directly modified with the
\code{()=} operator. For example \code{v()=3} replaces the current
value of the \code{v} var with \code{3}. Therefore, vars are changed
imperatively by the programmer.

