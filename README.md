# REScala

REScala -  Bridging Between Object-oriented and Functional Style in Reactive Applications

Traditionally, object-oriented software adopts the Observer pattern to implement reactive behavior. Its drawbacks are well-documented and two families of alternative approaches have been proposed, extending object-oriented languages with concepts from functional reactive and dataflow programming, respectively event-driven programming. The former hardly escape the functional setting; the latter do not achieve the declarativeness of more functional approaches.

REScala ia a reactive language which integrates concepts from event-based and functional-reactive programming into the object-oriented world. REScala supports the development of reactive applications by fostering a functional declarative style which complements the advantages of object-oriented design.

# A Reactive Language for the Object-oriented World

Software applications must react to external changes such as the input from the user and network messages. Traditionally, object-oriented software adopts the Observer pattern to implement reactivity and decouple the observers from the observables. Whereas researchers have highlighted the drawbacks of this style for a long time, alternatives struggle to be widely accepted. In particular, functional reactive programming and dataflow programming – which aim to represent time-changing values as first class abstractions – are promising, but hardly escape the functional setting. On the other hand, event-based languages directly support events but do not achieve the declarative style of more functional approaches.
REScala is a reactive language which integrates concepts from event-based and functional-reactive programming into the object-oriented world. REScala supports the development of reactive applications by fostering a functional and declarative style which complements the advantages of object-oriented design. Events and signals have their advantages and disadvantages and event-based applications cannot be refactored to use only signals without loss of desired properties. We derive that there is a need for a language design that supports a fluid transition between the two worlds and seamlessly integrates them into the OO setting.
REScala provides a rich API of functions for converting events to signals and the other way around. The goal is to ensure that the same abstraction/composition mechanisms uniformly apply over them. Conversion functions also facilitate refactoring of code fragments from one style to the other.

# The Role of Conversion Functions

![Reactive Graph](http://www.guidosalvaneschi.com/rescala/main/images/rescala-transformations.png)

We graphically depict event-based applications as a graph (Figure a), in which the nodes without a predecessor denote directly triggered events on which other events (indirectly) depend (inner nodes of the graph). Functions converting from events to signals are used to refactor some reactive functionality to signals, in cases when reactivity originates from events, graphically depicted in Figure c. Functions converting from a signal to an event are used when some piece of reactive functionality that is refactored to use signals still needs to interface to events, graphically depicted in Figure a.


# Signals

In REScala, the general form of a signal `s` is `Signal{expr}`, where `expr` is a standard Scala expression. When `expr` is evaluated, all `Signal` and `Var` values it refers to are registered as dependents of `s`; any subsequent change of them triggers a reevaluation of `s`.

```scala
	  val a = Var(2)
	  val b = Var(3)
	  val s = Signal{ a() + b() }
	  println(a.getVal,b.getVal,s.getVal) // (2,3,5)
          a()= 4
          println(a.getVal,b.getVal,s.getVal) // (4,3,7)
```

REScala signals integrate seamlessly with OO design. They are class attributes like fields and methods. They too can have different visibilities. Public signals are part of the class interface: Clients can refer to them to build composite reactive values. Conversely, private signals are only for object-internal use. REScala signals cannot be re-assigned new expressions once they are initialized.

```scala
	trait Animal {
	  val age: Signal[Int]
	  val name: Signal[String]
	  private[myPackage] val speed: Signal[Double]
	}
```

# Events

REScala supports a rich event system. Imperative events are fired directly by the programmer.

```scala
	  val e = new ImperativeEvent[Int]()
	  e += { x => println(x) }
	  e(10)
	  e(10)
	  // − output −
	  10
	  10
```

Declarative events, are defined as a combination of other events. For this purpose it offers operators like `e1 ||e2` (occurrence of one among `e1` or `e2` ), `e1 && p` (`e1` occurs and the predicate `p` is satisfied), `e1.map(f)` (the event obtained by applying `f` to `e1`). Event composition allows one to express the application logic in a clear and declarative way

```scala
	  val e1 = new ImperativeEvent[Int]()
	  val e2 = new ImperativeEvent[Int]()
	  val e1 OR e2 = e1 | | e2
	  e1 OR e2 += ((x: Int) => println(x))
	  e1(10)
	  e2(10)
	  // − output −
	  10
	  10
```

# Conversions

Since REScala promotes a mixed OO and functional style, it is important to manage state at the boundary between imperative and functional fragments of applications. For this purpose, REScala provides a set of functions for converting events into signals and vice versa.
The following code abstract over state to count the number of mouse clicks using the fold function.

```scala
val click: Event[(Int, Int)] = mouse.click
val nClick: Signal[Int] = click.fold(0)( (x, ) => x+1 )
The following code provides the position of the last click combining the click event and the position signal with the snapshot function.
val clicked: Event[Unit] = mouse.clicked
val position: Signal[(Int,Int)] = mouse.position
val lastClick: Signal[(Int,Int)] = position snapshot clicked
```

# Case Studies

## Universe

![Universe](http://www.guidosalvaneschi.com/rescala/main/images/universe.png)

Universe is a program that simulates a 2D environment. The environment is populated by animals and plants; the simulation involves growing of animals and plants, movements of animals, and planning for food search.
The simulation evolves in rounds and the state of each element at a given step is a function of the other elements and of the state of the simulation in the previous step. This structure allows one to express several aspects of the computation functionally. However, the elements of the simulation are mutable objects that encapsulate state, so the OO and the functional style must be properly combined.

<https://github.com/guidosalva/universe>

## React Edit

![React Edit](http://www.guidosalvaneschi.com/rescala/main/images/editor.png)

ReactEdit is a minimal text editor implementing functionalities like text selection, line counting, and cutting-and-pasting of text.
ReactEdit is a minimal version of the Editor widget in the SWT library, which is malleable to investigating various design alternatives based on reactive abstractions.

<https://github.com/guidosalva/editor>

## ReactRSS

![React RSS](http://www.guidosalvaneschi.com/rescala/main/images/rssreader.png)

ReactRSS is a RSS feed reader displaying a list of channels, which are periodically checked for updates.
Fetched items are immediately displayed to the user in a side bar. When the user selects one of them, the HTML content is rendered in the main view.

<https://github.com/guidosalva/rssreader>

## React Shapes

![React Shapes](http://www.guidosalvaneschi.com/rescala/main/images/shapes.png)

ReactShapes is a small drawing program. The user can drag and drop different shapes on a canvas, connect them with lines and change the stroke width and the color of each shape.
The application supports an history and an undo function. Finally, the drawing canvas can be shared with other clients that participate in the same task from remote.

<https://github.com/guidosalva/shapes>

# Related Things

* Homepage: <http://www.guidosalvaneschi.com/rescala/main/index.html>
* PDF Manual: <http://www.guidosalvaneschi.com/rescala/main/manual.pdf>
* Examples: <https://github.com/guidosalva/examples>
