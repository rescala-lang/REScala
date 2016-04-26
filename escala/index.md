---
layout: default
title: ESCala
---
# EScala
EScala is an extension of Scala programming language with support for events as attributes of objects. The support for events in EScala, combine the ideas of event-driven, aspect-oriented and functional-reactive programming.

Events are natural abstractions for describing interactive behavior, and the interfaces of objects. In conventional object-oriented languages events are usually modelled indirectly using some form of the Observer or the Publish-Subscribe pattern. For example, in Java libraries an object exposes events by providing a possibility to register so-called listener objects. In C# such pattern is supported directly in the language, by allowing to declare events as special class members. Despite their differences, in all these approaches the occurrences of events are defined imperatively by triggering them at the corresponding locations within the program.

EScala extends the idea of events as object members, as realized by C# events, with the possibility to define events declaratively by expressions over other events. The occurrences of an event can be defined by various set operations, such as union, intersection and difference, applied on the occurrences of other events. Events can be filtered by arbitrary conditions, the data attached to the events can be transformed by arbitrary functions. Event expressions make it possible to define events in terms of other events, at the lowest level relying on primitive events.

There are two kinds of primitive events in EScala: In addition to imperative events, which are defined by triggering, EScala also supports implicit events, which mark language-specific actions during the execution of a program, such as the beginning or the end of the execution of a method. Implicit events in ESCALA are analogous to join points in aspect-oriented languages.

EScala events are fully integrated with object-oriented features. An events is defined in the context of its owner object and can use their state and functionality in their definition. Events are inherited in subclasses, and the access to events is late-bound. Unlike typical aspect-oriented languages, EScala preserves object-oriented encapsulation and modular reasoning. Like in Java, classes can be compiled and loaded independently.

This is joint work with the [ASCOLA Research Group](http://www.emn.fr/z-info/ascola/)


# Introduction
EScala is an extension of the Scala programming language which supports events as attributes of objects. In EScala, events can be defined declaratively by expressions combining other events, or imperatively as realized in C#.

This section gives an overview of the language features and how to use them.

# Event Declaration
An event can be declared as member of a class using the evt keyword. An event can either be declared as imperative, i.e. occurrences are imperatively triggered in code (as in C#), or defined declaratively with an expression.

```scala
class Figure {
 imperative evt changed[Unit]
}
```
In this example, the class Figure defines an imperative event named changed, which provides no data with its occurrences (Unit type). Occurrences of an imperative event can then be imperatively created in the code by calling the event as a method.

changed() // create an occurrence of the changed event
Another way to define an event is to declaratively define it with an expression. EScala provides a way to combine events together using different types of operators:

* **`e || e'` (event disjunction)** The resulting event is triggered whenever `e` **or** `e'` occurs.
* **`e && e'` (event conjunction)** The resulting event is triggered whenever `e` **and** `e'` occur simultaneously.
* **`e && f` (event filtering)** The resulting event is triggered whenever `e` occurs and the given filter function `f` evaluates to `true`.
* **`e.map(f)` (parameter mapping)** The resulting event is triggered whenever `e` occurs, and the parameter types and value are transformed by the function `f` passed to the `map` operator (We will explain it in more details below).
So we could define different events in the Figure class using these combinators

```scala
class Figure {
  imperative evt resized[Unit]
  imperative evt moved[Unit]
  evt changed = resized || moved
}
```
The changed event in this example is defined as the disjunction of two other events named resized and moved. The type parameter declaration can be omitted in the definition of changed and will be inferred by the compiler.


# Examples
EScala can be used as a standard Scala library. This section shows examples on how to use the library with an unmodified version of the language. A complete example is available for download in the Download section.

The event library classes are all in the `events.lib` package.

## Event declaration
All the event are a subtype of `events.lib.Event[T]`.

Events are declared as values

```scala
import events.lib._

class Figure {
  lazy val changed = new ImperativeEvent[Unit]
}
```
This declares an imperative event which takes Unit as parameter, i.e. no parameter. This event can be explicitly triggered in the code as a method call.

```scala
val f = new Figure
// trigger the changed event
f.changed()
```
Events can also be combined together using the combinator methods available in the Event trait

```scala
class Figure {
  // event emitting two integers
  lazy val figureMoved = new ImperativeEvent[(Int, Int)]
  // event emitting an integer
  lazy val colorChanged = new ImperativeEvent[Int]
  // disjunction of both previously declared events
  // parameters are dropped to unify the event types
  lazy val changed: Event[Unit] = figureMoved.dropParam || colorChanged.dropParam
}
```
Some of the available combinators are

* Events disjunction e1 || e2
  Both event types must be compatible.
* Events conjunction e1 && e2
  Event types are merged to a pair.
* Event filter e && predicate
  predicate is a method returning a boolean. The resulting event matches only if teh event was triggered and the predicate was true.
* Parameter mapping e.map(transformation)
  It allows to map the value provided by the event to an other type or value. The resulting event parameter has for type the return type of the transformation method
* Parameter droping e.dropParam
 Equivalent to the map operator with a transformation method doing nothing and returning Unit

## Reactions registration
Reactions can be registered to an event by using the += operator

```scala
class Figure {
  …
  def reaction() {
  // react to the event
  }
  changed += reaction
```
The reaction must take as parameter the value provided by the observed event.

It is also possible to deregister a reaction using the -= operator. In this case the reaction method must be declared as lazy value.

```scala
class Figure {
 …
 lazy val reaction = () => {
 // do something
 }
 changed += reaction
 def undeploy {
 changed -= reaction
 }
}
```

## Variable references
The library provides two utility classes allowing to reference events of mutable variables

`Variable[T]` wraps a variable of type `T`. It notifies the changes of the reference through a changed event. It also provides an event operator which takes a function computing an event from the wrapped object as parameter and returns the event depending on the object.
`VarList[T]` is an observable list. It provides a method any which aggregates the events of the elements in the list, described by a function passed as parameter. The list observes changes in its content, which allows to aggregate the events of its current elements.

## Observable methods
Implicit events can be observed by lifting a method to make it observable

```scala
class Figure {
  lazy val moveBy = Observable((dx: Int, dy: Int) => {
  // move the figure
  })
}
```
Such a lifted methods exposes then two implicit events named before and after which are triggered before reps. after the method call. They can be used to declare more complex events.

```scala
class Figure {
  lazy val figureMoved: Event[(Int, Int)]= moveBy.after
}
```

## Implicit conversions
The event library contains an object named EventsLibConversions providing useful implicit conversions. Among others it provides following conversion

* Wrapping an object of type `T` into an object of type `Variable[T]`
* Converting a `Function1[T,R]` object to a corresponding `Observable[T,R]` object
* Converting a `FunctionN` object to the corresponding tupled function or a `Function0[R]` object to the corresponding `Function1[Unit, R]` object.

# Downloads

| EScala compiler and library binaries                   | [EScala 0.3 distribution](http://www.stg.tu-darmstadt.de/media/st/research/escala/escala_03.zip)                              |
| Universe case study (EScala)                           | [Universe source code](http://www.stg.tu-darmstadt.de/media/st/research/escala/examples/universe_src.zip)                     |
| EScala figure example                                  | [Figure example](http://www.stg.tu-darmstadt.de/media/st/research/escala/examples/figure_src.scala)                           |
| Standalone EScala library (compiled against scala 2.8) | [Events library](http://www.stg.tu-darmstadt.de/media/st/research/escala/examples/event_library.jar)                          |
| EScala library source                                  | [Library source code](http://www.stg.tu-darmstadt.de/media/st/research/escala/examples/event_library_src.jar)                 |
| Example using the library in standard Scala            | [Example using the library](http://www.stg.tu-darmstadt.de/media/st/research/escala/examples/event_library_example_src.scala) |
| REScala (including examples)                           | [REScala distribution](http://www.stg.tu-darmstadt.de/media/st/research/escala/rescala.zip)                                   |
| Universe case study (REScala)                          | [Universe source code (REScala)](http://www.stg.tu-darmstadt.de/media/st/research/escala/examples/universe_rescala_src.zip)   |
