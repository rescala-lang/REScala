---
title: Datastructures
sidebar: rescala
---
# Reactive Datastructures

Reactive Datastructures is a library of datastructures
based on REScala reactive abstractions. The library is
[compatible with the Scala collection framework](http://docs.scala-lang.org/overviews/core/architecture-of-scala-collections.html).
Attributes are exposed as reactive values
and operations on the structure are updated reactively.

```scala
val b = Var(2)
val a = Var(3)
collection += 1
collection += b.toSignal
collection += a.toSignal

val filtered: ReactiveHashSet[Int] =
collection.filter(Var((x: Int) => x % 2 == 0).toSignal)
assertResult(true)(filtered.contains(2)())
assertResult(false)(filtered.contains(3)())
assertResult(false)(filtered.contains(4)())

// filtered.contains is a Signal!
collection += 4
assertResult(true)(filtered.contains(4)())
b() = 3
assertResult(false)(filtered.contains(2)())
a() = 2
assertResult(true)(filtered.contains(2)())
```

The Reactive Datastructures on GitHub:

[https://github.com/guidosalva/reactive-datastructures](https://github.com/guidosalva/reactive-datastructures)


The Reactive Datastructures library has been provided by
Yannick Schrockers.
