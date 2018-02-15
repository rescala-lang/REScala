---
title: Manual
version: 0.4
nav: 3
sidebar: manual
---

# Quickstart with sbt

Create a `build.sbt` file in an empty folder with the following contents:

```scala
scalaVersion := "2.12.4"

resolvers += Resolver.bintrayRepo("stg-tud", "maven")

libraryDependencies += "de.tuda.stg" %% "rescala" % "0.21.0"
```

Install [sbt](http://www.scala-sbt.org/) and run `sbt console` inside the folder,
this should allow you to follow along the following examples.

# API Documentation

* [Signal documentation](../scaladoc/rescala/reactives/Signal.html)
* [Event documentation](../scaladoc/rescala/reactives/Event.html)


# Introduction

This manual covers the main features of the *Rescala* programming language.
[Signals and Vars] presents time-changing values
in *Rescala*, [Events](#events) describes events,
[Conversion Functions](#conversion-functions) covers the conversion functions between
events and time-changing values, [Technicalities](#technicalities)
presents technical details that are necessary to correctly run
*Rescala*, [Related](#related) outlines the related work.

While a major aspect of *Rescala*'s design is the integration of events
and signals, they can be used separately. For example a programmer can
use only *Rescala* events to design application that do not need
time-changing values.

**Scope** The manual serves as an introduction of the concepts in *Rescala*.
The full API is covered in the [scaladoc](../scaladoc/) especially for Signals and Events (see above for direct links).
More details can be found in [[7, 3]](#ref).

The manual introduces the concepts related to functional reactive
programming and event-based programming from a practical
perspective. The readers interested in a more general presentation of
these topics can find thee essential
references in the [related work](#related).

The code examples in the manual serve as a self contained Scala REPL session,
all code is executed and results are annotated as comments using [tut](https://github.com/tpolecat/tut).
To use all features of *Rescala* the only required import is:

```tut:book
import rescala._
```

Most code blocks can be executed on their own when adding this import,
but some require definitions from the prior blocks.


# Signals and Vars
[Signals and Vars]: #signals-and-vars

A signal expresses functional dependencies among values.
Intuitively, the value of a signal is computed from one or multiple input values.
Whenever any inputs changes, the value of the signal is also updated.

For example:

```tut:book
val a = Var(2)
val b = Var(3)
val c = Signal { a() + b() }
println((a.now, b.now, c.now))
a set 4
println((a.now, b.now, c.now))
b set 5
println((a.now, b.now, c.now))
```

In the code above, the signal `c` is defined to be `a + b` (details on syntax follows in the next section).
When `a` or `b` are updated, the value of `c` is updated as well.


## Vars

A `Var[T]` holds a simple value of type `T` and does not have any inputs.
`Var[T]` is a subtype of `Signal[T]` and can be used as an input for any signal.
Examples for var declarations are:

```tut:book
val a = Var(0)
val b = Var("Hello World")
val c = Var(List(1,2,3))
val d = Var((x: Int) => x * 2)
```

Vars enable the framework to track changes of input values.
Vars can be changed directly by the programmer:

```tut:book
a set 10
b.set("The `set` method does the same as the update syntax above")
c.transform( list => 0 :: list )
```

Vars are used by the framework to track changes to inputs,
the value of a var must not be mutated indirectly,
as such changes are hidden to the framework.


## Signals

### Defining Signals
 Signals are defined by the syntax
```Signal{```*sigexpr*```}```, where *sigexpr* is a side
effect-free expression. Signals are parametric types. A signal that
carries integer values has the type ```Signal[Int]```.

### Signal expressions
 When, inside a signal expression
defining a signal ```s```, a var or a signal is called with the
```()``` operator, the var or the signal are added to the values
```s``` depends on. In that case, ```s``` *is a dependency* of
the vars and the signals in the signal expression. For example in the
code snippet:

```tut:book
  val a = Var(0)
  val b = Var(0)
  val s = Signal{ a() + b() } // Multiple vars in a signal expression
```

The signal ```s``` is a dependency of the vars ```a``` and ```b```,
meaning that the values of ```s``` depends on both ```a``` and
```b```. The following code snippets define valid signal
declarations.

```tut:book
val a = Var(0)
val b = Var(0)
val c = Var(0)
val r: Signal[Int] = Signal{ a() + 1 } // Explicit type in var decl
val s = Signal{ a() + b() } // Multiple vars is a signal expression
val t = Signal{ s() * c() + 10 } // Mix signals and vars in signal expressions
val u = Signal{ s() * t() } // A signal that depends on other signals
```

```tut:book
val a = Var(0)
val b = Var(2)
val c = Var(true)
val s = Signal{ if (c()) a() else b() }
```

```tut:book
def factorial(n: Int) = Range.inclusive(1,n).fold(1)(_ * _)
val a = Var(0)
val s: Signal[Int] = Signal{ // A signal expression can be any code block
  val tmp = a() * 2
  val k = factorial(tmp)
  k + 2  // Returns an Int
}
```



### Accessing reactive values
 The current value of a
signal or a var can be accessed using the ```now``` method. For
example:

```tut:book
val a = Var(0)
val b = Var(2)
val c = Var(true)
val s: Signal[Int] = Signal{ a() + b() }
val t: Signal[Boolean] = Signal{ !c() }
val x: Int = a.now
val y: Int = s.now
val z: Boolean = t.now
println(z)
```

## Example: speed
The following example computes the displacement `space` of a
particle that is moving at constant speed `SPEED`. The
application prints all the values associated to the displacement over
time.

```tut:book
val SPEED = 10
val time = Var(0)
val space = Signal{ SPEED * time() }

space.changed += ((x: Int) => println(x))

while (time.now < 5) {
  Thread sleep 20
  time set time.now + 1
}
```

The application behaves as follows. Every 20 milliseconds, the value
of the `time` var is increased by 1 (Line 9).
When the value of the `time` var changes, the signal expression
at Line 3 is reevaluated and the value of `space` is
updated. Finally, the current value of the `space` signal is
printed every time the value of the signal changes.

Printing the value of a signal deserves some more considerations.
Technically, this is achieved by converting the ```space``` signal to
an event that is fired every time the signal changes its value
(Line 5). The conversion is performed by the
`changed` operator. The `+=` operator attaches an handler to
the event returned by the `changed` operator. When the event
fires, the handler is executed. Line 5 is equivalent to
the following code:

```tut:book
val e: Event[Int] = space.changed
val handler:  (Int => Unit) =  ((x: Int) => println(x))
e observe handler
```

Note that using `println(space.now)` would also print the
value of the signal, but only at the point in time in which the print
statement is executed. Instead, the approach described so far prints
*all* values of the signal. More details about converting signals
into events and back are provided in [Conversion Functions](#conversion-functions).

---

# Events

*Rescala* supports different kind of events. Imperative events are
directly triggered from the user. Declarative events trigger when the
events they depend on trigger. In reactive applications, events are
typically used to model changes that happen at discrete points in
time. For example a mouse click from the user or the arrival of a new
network packet. Some features of *Rescala* events are valid for all
event types.


* Events carry a value. The value is associated to the event when
  the event is fired and received by all the registered handlers when
  each handler is executed.

* Events are generic types parametrized with the type of value
  they carry, like `Event[T]` and `Evt[T]` where
  `T` is the value carried by the event.

* Both imperative events and declarative events are subtypes of
  `Event[T]` and can referred to generically.


# Imperative events

*Rescala* imperative events are triggered imperatively by the
programmer. One can think to imperative events as a generalization of
a method call which supports (multiple) bodies that are registered and
unregistered dynamically.

## Defining  Events

Imperative events are defined by the `Evt[T]`
type. The value of the parameter `T` defines the value that is
attached to the event. An event with no parameter attached has
signature `Evt[Unit]`. The following code snippet show
valid events definitions:

```tut:book
val e1 = Evt[Unit]()
val e2 = Evt[Int]()
val e3 = Evt[String]()
val e4 = Evt[Boolean]()
val e5: Evt[Int] = Evt[Int]()
class Foo
val e6 = Evt[Foo]()
```

It is possible to attach more than one value to the same event. This
is easily accomplished by using a tuple as a generic parameter
type. For example:

```tut:book
val e1 = Evt[(Int,Int)]()
val e2 = Evt[(String,String)]()
val e3 = Evt[(String,Int)]()
val e4 = Evt[(Boolean,String,Int)]()
val e5: Evt[(Int,Int)] = Evt[(Int,Int)]()
```

Note that an imperative event is also an event. Therefore the
following declaration is also valid:

```tut:book
val e1: Event[Int] = Evt[Int]()
```

## Registering Handlers

Handlers are code blocks that are executed when the event fires. The
`+=` operator attaches the handler to the event. The handler is a
first class function that receives the attached value as a parameter.
The following are valid handler definitions.

```tut:book
var state = 0
val e = Evt[Int]()
e += { println(_) }
e += (x => println(x))
e += ((x: Int) => println(x))
e += (x => {  // Multiple statements in the handler
  state = x
  println(x)
})
```

The signature of the handler must conform the signature of the event,
since the handler is supposed to process the attached value and
perform side effects. For example is the event is of type
`Event[(Int,Int)]` the handler must be of type `(Int,Int) => Unit`.

```tut:book
val e = Evt[(Int,String)]()
e += (x => {
  println(x._1)
  println(x._2)
})
e += ((x: (Int,String)) => {
  println(x)
})
```

Note that events without arguments still need an argument
in the handler.

```tut:book
val e = Evt[Int]()
e += { x => println() }
e += { (x: Int) => println() }
```

Scala allows one to refer to a method using the partially applied
function syntax. This approach can be used to directly register a
method as an event handler. For example:

```tut:book
def m1(x: Int) = {
  val y = x + 1
  println(y)
}
val e = Evt[Int]
e += m1 _
e.fire(10)
```

## Firing Events

Events can be fired with the same syntax of a method call. When an
event is fired, a proper value must be associated to the event
call. Clearly, the value must conform the signature of the event. For
example:

```tut:book
val e1 = Evt[Int]()
val e2 = Evt[Boolean]()
val e3 = Evt[(Int,String)]()
e1.fire(10)
e2.fire(false)
e3.fire((10,"Hallo"))
```

When a handler is registered to an event, the handler is executed
every time the event is fired. The actual parameter is provided to the
handler.

```tut:book
val e = Evt[Int]()
e += { x => println(x) }
e.fire(10)
e.fire(10)
```

If multiple handlers are registered, all of them are executed when the
event is fired. Applications should not rely on any specific execution
order for handler execution.

```tut:book
val e = Evt[Int]()
e += { x => println(x) }
e += { x => println(f"n: $x")}
e.fire(10)
e.fire(10)
```

## Unregistering Handlers

Handlers can be unregistered from events with the `remove`
operator. When a handler is unregistered, it is not executed when the
event is fired.

```tut:book
val e = Evt[Int]()

val handler1 = e += println
val handler2 = e += { x => println(s"n: $x") }
e.fire(10)
handler2.remove()
e.fire(10)
handler1.remove()
e.fire(10)
```

# Declarative Events

*Rescala* supports declarative events, which are defined as a
combination of other events. For this purpose it offers operators like
`e_1 || e_2` , `e_1 && p` , `e_1.map(f)`. Event composition allows to
express the application logic in a clear and declarative way. Also,
the update logic is better localized because a single expression
models all the sources and the transformations that define an event
occurrence.

## Defining Declarative Events

Declarative events are defined by composing other events. The
following code snippet shows some examples of valid definitions for
declarative events.

```tut:book
val e1 = Evt[Int]()
val e2 = Evt[Int]()

val e3 = e1 || e2
val e4 = e1 && ((x: Int)=> x>10)
val e5 = e1 map ((x: Int)=> x.toString)
```

# Event Operators

This section presents in details the operators that allow one to
compose events into declarative events.

## OR Events

The event `e_1 || e_2` is fired upon the occurrence of one among `e_1`
or `e_2`. Note that the events that appear in the event expression
must have the same parameter type (`Int` in the next example).

```tut:book
val e1 = Evt[Int]()
val e2 = Evt[Int]()
val e1_OR_e2 = e1 || e2
e1_OR_e2 += ((x: Int) => println(x))
e1.fire(1)
e2.fire(2)
```

## Filtering (Predicate) Events

The event `e filter p` (or the alternative syntax `e && p`) is fired if `e` occurs and the predicate `p` is
satisfied. The predicate is a function that accepts the event
parameter as a formal parameter and returns `Boolean`. In other
words the filter operator filters the events according to their
parameter and a predicate.

```tut:book
val e = Evt[Int]()
val e_AND: Event[Int] = e filter ((x: Int) => x>10)
e_AND += ((x: Int) => println(x))
e.fire(5)
e.fire(15)
```

## Map Events

The event `e.map f` is obtained by applying `f` to the value carried
by `e`. The map function must take the event parameter as a formal
parameter. The return type of the map function is the type parameter
value of the resulting event.

```tut:book
val e = Evt[Int]()
val e_MAP: Event[String] = e map ((x: Int) => x.toString)
e_MAP += ((x: String) => println(s"Here: $x"))
e.fire(5)
e.fire(15)
```

{::comment}
## dropParam

The `dropParam` operator transforms an event into an event with
`Unit` parameter. In the following example the `dropParam`
operator transforms an `Event[Int]` into an `Event[Unit]`.

```tut:book
val e = Evt[Int]()
val e_drop: Event[Unit] = e.dropParam
e_drop += (_ => println("*"))
e.fire(10)
e.fire(10)
```

The typical use case for the `dropParam` operator is to make events
with different types compatible. For example the following snippet is
rejected by the compiler since it attempts to combine two events of
different types with the `||` operator.

```tut:nofail
/* WRONG - DON'T DO THIS */
val e1 = Evt[Int]()
val e2 = Evt[Unit]()
val e1_OR_e2 = e1 || e2  // Compiler error
```

The following example is correct. The `dropParam` operator allows
one to make the events compatible with each other.

```tut:book
val e1 = Evt[Int]()
val e2 = Evt[Unit]()
val e1_OR_e2: Event[Unit] = e1.dropParam || e2
```
{:/comment}

---

# Conversion Functions

*Rescala* provides functions that interface signals and
events. Conversion functions are fundamental to introduce
time-changing values into OO applications -- which are usually
event-based.

# Basic Conversion Functions

This section covers the basic conversions between signals and events.
Figure 1 shows how basic conversion functions can
bridge signals and events. Events (Figure 1,
left) occur at discrete point in time (x axis) and have an associate
value (y axis). Signals, instead, hold a value for a continuous
interval of time (Figure 1, right). The
`latest` conversion functions creates a signal from an event. The
signal holds the value associated to an event. The value is hold until
the event is fired again and a new value is available. The
`changed` conversion function creates an event from a signal. The
function fires a new event every time a signal changes its value.

<figure markdown="1">
![Event-Signal](./images/event-signal.png)
<figcaption>Figure 1: Basic conversion functions.</figcaption>
</figure>

## Event to Signal: Latest

Returns a signal holding the latest value of the event `e`. The
initial value of the signal is set to `init`.

`latest[T](e: Event[T], init: T): Signal[T]`

Example:

```tut:book
val e = Evt[Int]()
val s: Signal[Int] = e.latest(10)
assert(s.now == 10)
e.fire(1)
assert(s.now == 1)
e.fire(2)
assert(s.now == 2)
e.fire(1)
assert(s.now == 1)
```

## Signal to Event: Changed

The `changed` function applies to a signal and returns an event
that is fired every time the signal changes its value.

`changed[U >: T]: Event[U]`

Example:

```tut:book
var test = 0
val v =  Var(1)
val s = Signal{ v() + 1 }
val e: Event[Int] = s.changed
e += ((x:Int)=>{test+=1})
v.set(2)
assert(test == 1)
v.set(3)
assert(test == 2)
```

## Fold

The `fold` function creates a signal by folding events with a
given function. Initially the signal holds the `init`
value. Every time a new event arrives, the function `f` is
applied to the previous value of the signal and to the value
associated to the event. The result is the new value of the signal.

`fold[T,A](e: Event[T], init: A)(f :(A,T)=>A): Signal[A]`

Example:

```tut:book
val e = Evt[Int]()
val f = (x:Int,y:Int)=>(x+y)
val s: Signal[Int] = e.fold(10)(f)
e.fire(1)
e.fire(2)
assert(s.now == 13)
```

## Fold matchers

The `fold` `Match` construct allows to match on one of multiple events.
For every firing event, the corresponding handler function is executed,
to compute the new state.
If multiple events fire at the same time,
the handlers are executed in order.
The acc parameter reflects the current state.


```tut:book
val word = Evt[String]
val count = Evt[Int]
val reset = Evt[Unit]

val result = Events.fold(""){ acc => Events.Match(
  reset >> (_ => ""),
  word >> identity,
  count >> (acc * _),
)}

result.observe(r => println(r))

count.fire(10)
reset.fire()
word.fire("hello")
count.fire(2)
word.fire("world")
update(count -> 2, word -> "do them all!", reset -> (()))
```

## Iterate

Returns a signal holding the value computed by `f` on the
occurrence of an event. Differently from `fold`, there is no
carried value, i.e. the value of the signal does not depend on the
current value but only on the accumulated value.

`iterate[A](e: Event[_], init: A)(f: A=>A): Signal[A]`

Example:

```tut:book
var test: Int = 0
val e = Evt[Int]()
val f = (x:Int)=>{test=x; x+1}
val s: Signal[Int] = e.iterate(10)(f)
e.fire(1)
assert(test == 10)
assert(s.now == 11)
e.fire(2)
assert(test == 11)
assert(s.now == 12)
e.fire(1)
assert(test == 12)
assert(s.now == 13)
```

## LatestOption

The `latestOption` function is a variant of the `latest`
function which uses the `Option` type to distinguish the case in
which the event did not fire yet. Holds the latest value of an event
as `Some(val)` or `None`.

`latestOption[T](e: Event[T]): Signal[Option[T]]`

Example:

```tut:book
val e = Evt[Int]()
val s: Signal[Option[Int]] = e.latestOption()
assert(s.now == None)
e.fire(1)
assert(s.now == Option(1))
e.fire(2)
assert(s.now == Option(2))
e.fire(1)
assert(s.now == Option(1))
```

## Last

The `last` function generalizes the `latest` function and
returns a signal which holds the last `n` events.

`last[T](e: Event[T], n: Int): Signal[List[T]]`

Initially, an empty list is returned. Then the values are
progressively filled up to the size specified by the
programmer. Example:

```tut:book
val e = Evt[Int]()
val s: Signal[scala.collection.LinearSeq[Int]] = e.last(5)

s observe println

e.fire(1)
e.fire(2)
e.fire(3);e.fire(4);e.fire(5)
e.fire(6)
```

## List

Collects the event values in a (growing) list. This function should be
used carefully. Since the entire history of events is maintained, the
function can potentially introduce a memory overflow.

`list[T](e: Event[T]): Signal[List[T]]`

## Count

Returns a signal that counts the occurrences of the event. Initially,
when the event has never been fired yet, the signal holds the value
0. The argument of the event is simply discarded.

`count(e: Event[_]): Signal[Int]`

```tut:book
val e = Evt[Int]()
val s: Signal[Int] = e.count
assert(s.now == 0)
e.fire(1)
e.fire(3)
assert(s.now == 2)
```

## Change

The ```change``` function is similar to ```changed```, but it
provides both the old and the new value of the signal in a tuple.

```change[U >: T]: Event[(U, U)]```

Example:

```tut:book
val s = Var(5)
val e = s.change
e += println

s.set(10)
s.set(20)
```

## ChangedTo

The ```changedTo``` function is similar to ```changed```, but it
fires an event only when the signal changes its value to a given
value.

```changedTo[V](value: V): Event[Unit]```

```tut:book
var test = 0
val v =  Var(1)
val s = Signal{ v() + 1 }
val e: Event[Unit] = s.changedTo(3)
e += ((x:Unit)=>{test+=1})

assert(test == 0)
v set(2)
assert(test == 1)
v set(3)
assert(test == 1)
```

## Switch/toggle

The ```toggle``` function switches alternatively between the given
signals on the occurrence of an event ```e```. The value attached to
the event is simply discarded.

```toggle[T](e : Event[_], a: Signal[T], b: Signal[T]): Signal[T]```

The `switchTo` function switches the value of the signal on the
occurrence of the event ```e```. The resulting signal is a constant
signal whose value is the value carried by the event ```e```.

```switchTo[T](e : Event[T], original: Signal[T]): Signal[T]```

The ```switchOnce``` function switches to a new signal provided as a
parameter, once, on the occurrence of the event ```e```.

`switchOnce[T](e: Event[_], original: Signal[T], newSignal: Signal[T]): Signal[T]`

## Flatten

The ```unwrap``` function is used to ``unwrap'' an event inside a signal.

```def unwrap[T](wrappedEvent: Signal[Event[T]]): Event[T]```

It can, for instance, be used to detect if any signal within a collection of signals
fired a changed event:

```tut:book
val v1 = Var(1)
val v2 = Var("Test")
val v3 = Var(true)
val collection: List[Signal[_]] = List(v1, v2, v3)
val innerChanges = Signal {collection.map(_.changed).reduce((a, b) => a || b)}
val anyChanged = innerChanges.flatten
anyChanged += println
v1.set(10)
v2.set("Changed")
v3.set(false)
```

---

# Common Pitfalls

In this section we
collect the most common pitfalls for users that are new to reactive
programming and *Rescala*.

## Accessing values in signal expressions

The ```()```
operator used on a signal or a var, inside a signal expression,
returns the signal/var value *and* creates a dependency. The
```now``` operator returns the current value but does *not*
create a dependency. For example the following signal declaration
creates a dependency between ```a``` and ```s```, and a dependency
between ```b``` and ```s```.

```tut:book
val s = Signal{ a() + b() }
```

The following code instead establishes only a dependency between
```b``` and ```s```.

```tut:book
val s = Signal{ a.now + b() }
```

In other words, in the last example, if ```a``` is updated, ```s```
is not automatically updated. With the exception of the rare cases in
which this behavior is desirable, using ```now``` inside a signal
expression is almost certainly a mistake. As a rule of dumb, signals
and vars appear in signal expressions with the ```()``` operator.


## Attempting to assign a signal
Signals are not
assignable. Signal depends on other signals and vars, the dependency
is expressed by the signal expression. The value of the signal is
automatically updated when one of the values it depends on
changes. Any attempt to set the value of a signal manually is a
mistake.


## Side effects in signal expressions
Signal expressions
should be pure. i.e. they should not modify external variables. For
example the following code is conceptually wrong because the variable
```c``` is imperatively assigned form inside the signal expression
(Line 4).

```tut:book:nofail
var c = 0                 /* WRONG - DON'T DO IT */
val s = Signal{
  val sum = a() + b();
  c = sum * 2
}
// …
println(c)
```

A possible solution is to refactor the code above to a more functional
style. For example, by removing the variable ```c``` and replacing it
directly with the signal.

```tut:book
val c = Signal{
  val sum = a() + b();
  sum * 2
}
// …
println(c.now)
```

## Cyclic dependencies
When a signal ```s``` is defined, a
dependency is establishes with each of the signals or vars that appear
in the signal expression of ```s```. Cyclic dependencies produce a
runtime error and must be avoided. For example the following code:

```tut:book:nofail
val a = Var(0)             /* WRONG - DON'T DO IT */
val s = Signal{ a() + t() }
val t = Signal{ a() + s() + 1 }
```

creates a mutual dependency between ```s``` and
```t```. Similarly, indirect cyclic dependencies must be avoided.

## Objects and mutability
Vars and signals may behave
unexpectedly with mutable objects. Consider the following example.

```tut:book:nofail
class Foo(init: Int){            /* WRONG - DON'T DO IT */
  var x = init
}
val foo = new Foo(1)

val varFoo = Var(foo)
val s = Signal{ varFoo().x + 10 }
// s.now == 11
foo.x = 2
// s.now == 11
```

One may expect that after increasing the value of ```foo.x``` in
Line 9, the signal expression is evaluated again and updated
to 12. The reason why the application behaves differently is that
signals and vars hold *references* to objects, not the objects
themselves. When the statement in Line 9 is executed, the
value of the ```x``` field changes, but the reference hold by the
```varFoo``` var is the same. For this reason, no change is detected
by the var, the var does not propagate the change to the signal, and
the signal is not reevaluated.

A solution to this problem is to use immutable objects. Since the
objects cannot be modified, the only way to change a filed is to
create an entirely new object and assign it to the var. As a result,
the var is reevaluated.

```tut:book
class Foo(val x: Int){}
val foo = new Foo(1)

val varFoo = Var(foo)
val s = Signal{ varFoo().x + 10 }
// s.now == 11
varFoo set (new Foo(2))
// s.now == 12
```

Alternatively, one can still use mutable objects but assign again the
var to force the reevaluation. However this style of programming is
confusing for the reader and should be avoided when possible.

```tut:book
class Foo(init: Int){   /* WRONG - DON'T DO IT */
  var x = init
}
val foo = new Foo(1)

val varFoo = Var(foo)
val s = Signal{ varFoo().x + 10 }
// s.now == 11
foo.x = 2
varFoo set foo
// s.now == 11
```

## Functions of reactive values
Functions that operate on
traditional values are not automatically transformed to operate on
signals. For example consider the following functions:

```tut:book
def increment(x: Int): Int = x + 1
```

The following code does not compile because the compiler expects an
integer, not a var as a parameter of the ```increment``` function. In
addition, since the ```increment``` function returns an integer,
```b``` has type ```Int```, and the call ```b()``` in the signal
expression is also rejected by the compiler.

```tut:book:nofail
val a = Var(1)           /* WRONG - DON'T DO IT */
val b = increment(a)
val s = Signal{ b() + 1 }
```

The following code snippet is syntactically correct, but the signal
has a constant value 2 and is not updated when the var changes.

```tut:book
val a = Var(1)
val b = increment(a.now) // b is not reactive
val s = Signal{ b + 1 } // s is a constant signal with value 2
```

The following solution is syntactically correct and the signal
```s``` is updated every time the var ```a``` is updated.

```tut:book
val a = Var(1)
val s = Signal{ increment(a()) + 1 }
```

---

# Essential Related Work
{: #related }

A more academic presentation of *Rescala* is in [[7]](#ref). A
complete bibliography on reactive programming is beyond the scope of
this work. The interested reader can refer
to[[1]](#ref) for an overview of reactive programming
and to[[8]](#ref) for the issues
concerning the integration of RP with object-oriented programming.


*Rescala* builds on ideas originally developed in
EScala [[3]](#ref) -- which supports
event combination and implicit events. Other reactive languages
directly represent time-changing values and remove inversion of
control. Among the others, we mention
FrTime [[2]](#ref) (Scheme),
FlapJax [[6]](#ref) (Javascript),
AmbientTalk/R [[4]](#ref) and
Scala.React [[5]](#ref) (Scala).

---

# Acknowledgments

Several people contributed to this manual with their ideas and
comments. Among the others Gerold Hintz and Pascal Weisenburger.

---

# References
{: #ref}
[1] E. Bainomugisha, A. Lombide Carreton, T. Van Cutsem, S. Mostinckx, and
W. De Meuter. A survey on reactive programming. ACM Comput. Surv. 2013.

[2] G. H. Cooper and S. Krishnamurthi. Embedding dynamic dataflow in a call-byvalue
language. In ESOP, pages 294–308, 2006.

[3] V. Gasiunas, L. Satabin, M. Mezini, A. Ńũnez, and J. Noýe. EScala: modular
event-driven object interactions in Scala. AOSD ’11, pages 227–240. ACM, 2011.

[4] A. Lombide Carreton, S. Mostinckx, T. Cutsem, and W. Meuter. Loosely-coupled
distributed reactive programming in mobile ad hoc networks. In J. Vitek, editor,
Objects, Models, Components, Patterns, volume 6141 of Lecture Notes in Computer
Science, pages 41–60. Springer Berlin Heidelberg, 2010.

[5] I. Maier and M. Odersky. Deprecating the Observer Pattern with Scala.react. Technical
report, 2012.

[6] L. A. Meyerovich, A. Guha, J. Baskin, G. H. Cooper, M. Greenberg, A. Bromfield,
and S. Krishnamurthi. Flapjax: a programming language for ajax applications.
OOPSLA ’09, pages 1–20. ACM, 2009.

[7] G. Salvaneschi, G. Hintz, and M. Mezini. Rescala: Bridging between objectoriented
and functional style in reactive applications. In Proceedings of the 13th
International Conference on Aspect-Oriented Software Development, AOSD ’14,
New York, NY, USA, Accepted for publication, 2014. ACM.

[8] G. Salvaneschi and M. Mezini. Reactive behavior in object-oriented applications:
an analysis and a research roadmap. In Proceedings of the 12th annual international
conference on Aspect-oriented software development, AOSD ’13, pages
37–48, New York, NY, USA, 2013. ACM.
