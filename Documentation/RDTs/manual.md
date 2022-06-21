# Delta CRDT Library Manual

This manual is an introduction to Delta CRDTs and our implementation of this concept. For more information about the API, see the scaladoc.

* The chapter [Why Delta CRDTs?](#why-delta-crdts) gives a brief explanation of the concepts behind Delta CRDTs and CRDTs in general.
* Chapter [Using Delta CRDTs](#using-delta-crdts) describes how to use this library in your applications.
* For advanced users, [Extending the Library](#extending-the-library) explains how you can define your own CRDTs.

### Credits

This library is in large parts an implementation of the Delta CRDTs proposed in [[1]](#references) and the modifications to the synchronization approach made in [[3]](#references). The RCounter data type is an implementation of the resettable counter described in [[4]](#references), and the implementation of the RGA data type was heavily inspired by [[5]](#references).

## Why Delta CRDTs?

CRDTs (Conflict-Free Replicated Data Types) are abstract data types made for local-first applications with replicated data. They guarantee that, no matter what changes are concurrently made to different replicas, the data on all replicas will eventually be the same after synchronization of the changes. Developers that use CRDTs can thus easily achieve eventual consistency in their applications without having to put much thought into complex synchronization mechanisms. For more details on CRDTs in general, see [[2]](#references).

Delta CRDTs are a variant of CRDTs that synchronize by periodically sending messages that contain only the last changes made to the data instead of the whole data itself. Compared to classic state-based CRDTs that always have to send their whole state, synchronization messages of delta CRDTs are smaller and can be processed faster. For more details on delta CRDTs, see [[1]](#references).

## Using Delta CRDTs

The interfaces of delta CRDTs are mostly very similar to the standard immutable data structures in Scala. For example, an AWSet is similar to a normal Scala Set in that it has methods for e.g. checking if an element is contained in the set and for adding and removing elements. All CRDTs in this library are immutable, i.e. mutating methods such as adding elements to a set do not change the CRDT instance itself but rather return a new instance that reflects these changes.

However, since CRDTs are supposed to be used for data that is replicated on multiple replicas, CRDTs require some additional methods for synchronizing changes between these replicas. There are many possible strategies for when and how to exchange delta states representing these changes. This library provides two options for synchronization, a "basic" and a "reactive" implementation.

### Basic Implementation

The "basic" implementation is based on the anti-entropy algorithm proposed in [[1, 3]](#references). Here, every delta that is created by a CRDT is passed on to an instance of the `AntiEntropy` class, which is responsible for ensuring reliable at-least-once delivery of these deltas to all known neighbor replicas in the network. To this end, the `AntiEntropy` class uses sequence numbers and acknowledgement messages to track which deltas have already been delivered to which neighbors. The `AntiEntropy` class also receives incoming changes from other replicas and passes them on to the CRDT.

Since this basic implementation is mostly meant for locally testing CRDTs, deltas are not actually sent over a real network. Instead, we use a mock `Network` class which simulates the behavior of an unreliable network that may randomly lose, duplicate or delay messages.

Let's have a look at a simple program that uses these classes. In this example, we will use and synchronize two instances of the `EWFlag` CRDT. An `EWFlag` (Enable-Wins Flag) models a simple flag (boolean value) that can be enabled and disabled. In the case that the flag is concurrently enabled and disabled then "enable wins", i.e. after synchronization the flag will be enabled on all replicas.

First, we will create a new mock network to send synchronization messages over:

```scala
val network = new Network(0, 0, 0)
```

The arguments to the `Network` constructor specify the likelihoods for losing, duplicating and delaying messages (more details in the scaladoc). Next, we need to create an instance of the `AntiEntropy` class for each CRDT instance that we want to create:

```scala
val aea = new AntiEntropy[EWFlag.State[DietMapCContext]]("a", network, mutable.Buffer("b"))
val aeb = new AntiEntropy[EWFlag.State[DietMapCContext]]("b", network, mutable.Buffer("a"))
```

The constructor for `AntiEntropy` requires the state type of the CRDT that it will manage as a type parameter. Since we are going to use `EWFlag` CRDTs, we use the `State` type defined in the `EWFlag` companion object. This state type is parameterized over the type of causal context (an internal data structure of the CRDT) it uses. If you want to know more about causal contexts then you can have a look at the scaladoc for `CContext` or read the explanation of causal CRDTs in [[1]](#references). The `AntiEntropy` constructor also expects three arguments: the id of its replica, the network instance that we just created and a Buffer containing the ids of other replicas that this `AntiEntropy` should exchange deltas with over the network.

Now we can finally create two instances of the `EWFlag` CRDT and bind them to their respective `AntiEntropy` instance:

```scala
val flagA0 = EWFlag(aea)
val flagB0 = EWFlag(aeb)
```

After first creating an `EWFlag` it will be disabled, we can check this using the `read` method:

```scala
println(flagA0.read) // false
println(flagB0.read) // false
```

Next, let's change the value of these flags:

```scala
val flagA1 = flagA0.enable()
val flagB1 = flagB0.disable()

println(flagA1.read) // true
println(flagB1.read) // false
```

As expected, enabling a flag sets its value to `true` and disabling a flag that was already disabled does not change its value, so it is still `false`. The two flags now have two different values, i.e. they have diverged. To make them reconcile and get back to a single common value, we need to exchange their changes (deltas) using the two `AntiEntropy` instances created above. These received changes are then applied to the CRDTs by calling their `processReceivedDeltas` method:

```scala
AntiEntropy.sync(aea, aeb)

val flagA2 = flagA1.processReceivedDeltas()
val flagB2 = flagB1.processReceivedDeltas()

println(flagA2.read) // true
println(flagB2.read) // true
```

After synchronization, both flags are enabled. This is because the `EWFlag` CRDT always resolves concurrent enable and disable operations by letting the enable operation "win".

Finally, here is the full executable code example including all necessary imports:

```scala
import rescala.extra.lattices.delta.CContext.DietMapCContext
import rescala.extra.lattices.delta.Codecs._
import rescala.extra.lattices.delta.crdt.basic.{AntiEntropy, EWFlag, Network}

import scala.collection.mutable

object BasicExample extends App {
  val network = new Network(0, 0, 0)

  val aea = new AntiEntropy[EWFlag.State[DietMapCContext]]("a", network, mutable.Buffer("b"))
  val aeb = new AntiEntropy[EWFlag.State[DietMapCContext]]("b", network, mutable.Buffer("a"))

  val flagA0 = EWFlag(aea)
  val flagB0 = EWFlag(aeb)

  println(flagA0.read) // false
  println(flagB0.read) // false

  val flagA1 = flagA0.enable()
  val flagB1 = flagB0.disable()

  println(flagA1.read) // true
  println(flagB1.read) // false

  AntiEntropy.sync(aea, aeb)

  val flagA2 = flagA1.processReceivedDeltas()
  val flagB2 = flagB1.processReceivedDeltas()

  println(flagA2.read) // true
  println(flagB2.read) // true
}
```

### "Reactive Implementation"

The second implementation can be actually used to synchronize CRDTs over a real network. Since this version was originally developed to allow integration of Delta CRDTs with [REScala](https://www.rescala-lang.com/), a reactive programming extension of Scala, I like to call this implementation the "reactive implementation" (although it has no reactive components itself).

In this implementation, CRDTs have a `deltaBuffer` field and an `applyDelta` method. Whenever the CRDT state is changed locally or through a received delta, the delta that initiated this change is stored in the `deltaBuffer` list. On the other hand, `applyDelta` should be called to apply received deltas. How and when deltas are taken from the `deltaBuffer` and how they are sent to and received by other replicas is entirely up to you. To ensure that the Delta CRDTs work correctly, your middleware must however fulfill three requirements:

* All deltas are reliable delivered at least once to all direct neighbors
* After reading the `deltaBuffer` it needs to be emptied using `resetDeltaBuffer` so that the deltas aren't read and sent multiple times
* When new replicas can be added to the network dynamically, they need to receive the full from state from other replicas so that they are initialized correctly

Apart from the propagation of deltas, the usage of CRDTs in this implementation is very similar to the basic implementation:

```scala
import rescala.extra.lattices.delta.CContext.DietMapCContext
import rescala.extra.lattices.delta.Codecs._
import rescala.extra.lattices.delta.crdt.reactive.EWFlag

// On replica A
object ReplicaA extends App {
  val flagA0 = EWFlag("a")
  println(flagA0.read) //false

  val flagA1 = flagA0.enable()
  println(flagA1.read) // true

  // send delta to B

  // receive delta from B in variable "delta"

  val flagA2 = flagA1.applyDelta(delta)
  println(flagA2.read) // true
}


// On replica B
object ReplicaB extends App {
  val flagB0 = EWFlag("b")
  println(flagB0.read) //false

  val flagB1 = flagB0.disable()
  println(flagB1.read) // false

  // send delta to A

  // receive delta from A in variable "delta"

  val flagB2 = flagB1.applyDelta(delta)
  println(flagB2.read) // true
}
```

### Selecting the Right CRDT for Your Problem

When you are planning to use a Delta CRDT in your application, it is important to know that the complexity of the modeled data structure and the type of operations available on it play a large role in the performance characteristics of the CRDT. Most notably, CRDTs that can not only grow (e.g. add elements to a set) but also shrink (e.g. remove elements from a set) require a larger internal representation and are generally a bit slower than their grow-only counterpart. Thus, to achieve the best performance for your application, you should always try to find the "minimal" CRDT that fulfills your needs. For example, if you need a replicated set data structure but you already know that you never remove any elements from the set, you should consider using a `GSet` (grow-only set) instead of an `AWSet` (Add-Wins Set).

## Extending the Library

This section is meant to help you if you want to define your own Delta CRDTs or if you want to adapt the way CRDTs work internally. Since extending the library requires a fair amount of knowledge about Delta CRDTs, we assume in this section that you have read [[1]](#references). If you haven't, please read the paper first before continuing in this manual.

In this library, CRDTs are compositions of an abstract class that defines their interface and a trait that defines how deltas are handled in the CRDT. If you want to add a new data structure to our collection of CRDTs you will need to define a new CRDT Interface, and if you want to adjust how deltas are applied and provided to the middleware, you can define a new trait for delta handling.

### CRDT Interfaces

The interface of a CRDT is defined in an abstract class and its companion object:

```scala
object GSetInterface {
  type State[E] = Set[E]

  trait GSetCompanion {
    type State[E] = GSetInterface.State[E]
  }

  def elements[E]: DeltaQuery[State[E], Set[E]] = state => state

  def insert[E](element: E): DeltaMutator[State[E]] = (_, _) => Set(element)
}

/** A GSet is a Delta CRDT modeling a simple grow-only set. */
abstract class GSetInterface[E, Wrapper] extends CRDTInterface[GSetInterface.State[E], Wrapper] {
  def elements: Set[E] = query(GSetInterface.elements)

  def insert(element: E): Wrapper = mutate(GSetInterface.insert(element))
}
```

The object consists of three parts:

```scala
type State[E] = Set[E]
```

First, we define the type of the internal state of the CRDT. We use a type alias because it makes the code more readable for large state types and because library users may have to provide this type to other classes that handle synchronization such as e.g. `AntiEntropy`.

```scala
trait GSetCompanion {
  type State[E] = GSetInterface.State[E]
}
```

Next, we define a trait that will be extended by the companion objects of the concrete implementations of this CRDT interface. This trait contains a copy of the state type alias so that it can be accessed more naturally (e.g. `GSet.State` instead of `GSetInterface.State`). If your CRDT defines any implicits that need to be imported for it to work correctly, these should also be put into the trait.

```scala
def elements[E]: DeltaQuery[State[E], Set[E]] = state => state

def insert[E](element: E): DeltaMutator[State[E]] = (_, _) => Set(element)
```

Finally, the behavior of the CRDT is defined through a collection of methods that produce `DeltaQuery`s or `DeltaMutators`. A `DeltaQuery[A, B]` is a function that takes a state of type `A` and returns some information about it that has type `B`.
A `DeltaMutator[A]` is a function that takes a replicaID and a state of type `A` and returns a delta that, when merged with the state, will result in some mutation.

The abstract class has to extend the `CRDTInterface` trait and uses the methods defined in the companion object to define the interface of the CRDT. Since methods that deal directly with the internal state are often a bit hard to get correct, it is a good idea to keep the number of methods in the companion object small and define easily derived methods only in the abstract class. For example, the prepend operation on a list can be implemented as an insert at index 0, so the companion object does not need a separate method for prepending elements.

The `query` and `mutate` methods that wrap the calls to the methods in the compantion object are defined in the `CRDTInterface` trait and are necessary to actually apply the generated `DeltaQuery` and `DeltaMutator` functions on the state of the CRDT. As a result, querying methods in this abstract abstract class have as return type the return type of the `DeltaQuery` whereas mutating methods have the return type `Wrapper`. `Wrapper` is a type parameter of this abstract class which will be instantiated by the type of the concrete CRDT class that extends this abstract class. This type parameter is necessary to make sure that calling mutators on a class that extends this abstract class returns an instance of the derived class instead of an instance of this abstract class.

```scala
/** A GSet is a Delta CRDT modeling a simple grow-only set. */
abstract class GSetInterface[E, Wrapper] extends CRDTInterface[GSetInterface.State[E], Wrapper] {
  def elements: Set[E] = query(GSetInterface.elements)

  def insert(element: E): Wrapper = mutate(GSetInterface.insert(element))
}
```

### Delta Handling

The way that a CRDT handles deltas is defined in a trait that also extends `CRDTInterface`. This is what this trait looks like for the reactive implementation:

```scala
trait ReactiveCRDT[State, Wrapper] extends CRDTInterface[State, Wrapper] {
  val deltaBuffer: List[Delta[State]]

  protected def copy(state: State = state, deltaBuffer: List[Delta[State]] = deltaBuffer): Wrapper

  override def applyDelta(delta: Delta[State])(implicit u: UIJDLattice[State]): Wrapper = delta match {
    case Delta(origin, deltaState) =>
      UIJDLattice[State].diff(state, deltaState) match {
        case Some(stateDiff) =>
          val stateMerged = UIJDLattice[State].merge(state, stateDiff)
          copy(state = stateMerged, deltaBuffer = Delta(origin, stateDiff) :: deltaBuffer)
        case None => this.asInstanceOf[Wrapper]
      }
  }

  def resetDeltaBuffer(): Wrapper = copy(deltaBuffer = List())
}
```

Most importantly, the trait has to override and define the `applyDelta` method, which is responsible for merging deltas produced locally or remotely with the local state. Additionally, it is responsible for making these deltas available to the synchronization mechanism. In the case of the reactive implementation, this means placing deltas into the `deltaBuffer`. Both the basic and the reactive implementation also first compute the difference of the delta and the state and only use the parts of the delta that will actually change the state. To create a new CRDT instance that reflects these changes, we recommend defining an abstract `copy` function that will be implemented by the concrete CRDTs. Finally, the trait should define any other fields or methods that are required for interfacing with the synchronization mechanism, such as the `resetDeltaBuffer` method that is necessary for the reactive implementation.

### Composition of CRDTs

To actually use your newly defined CRDT interface or trait for delta handling, you need to define concrete CRDTs inheriting these. As mentioned earlier, concrete CRDTs are defined as classes that extend a CRDT interface and a trait for delta handling, with the goal to allow multiple delta handling implementations to use the same interface. For example, here is the definition of a `GSet` that uses the `GSetInterface` and reactive delta handling:

```scala
class GSet[E](
    val state: State[E],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[E]]]
) extends GSetInterface[E, GSet[E]] with ReactiveCRDT[State[E], GSet[E]] {

  override protected def copy(state: State[E], deltaBuffer: List[Delta[State[E]]]): GSet[E] =
    new GSet(state, replicaID, deltaBuffer)
}

object GSet extends GSetCompanion {
  def apply[E](replicaID: String): GSet[E] = new GSet(UIJDLattice[State[E]].bottom, replicaID, List())
}
```

The `GSet` class implements the abstractly defined `state` and `replicaID` fields from `CRDTInterface` as well as the abstractly defined `deltaBuffer` field from `ReactiveCRDT` by simply taking them as constructor parameters. Then, after  implementing the copy method defined in `ReactiveCRDT`, the interface methods of the CRDT and the delta handling are simply inherited from the combination of `GSetInterface` and `ReactiveCRDT`. To make the creation of new CRDTs easier, we define an `apply` method in the companion object, which inherits from the `GSetCompanion` trait explained above.

Because of this compositional definition of CRDTs, creating an class that uses your new CRDT interface definition with the reactive implementation is as easy as defining a class that inherits from your abstract class and the `ReactiveCRDT` trait. On the other hand, to use your own trait for delta handling for a `GSet` CRDT you just need to define a class that extends `GSetInterface` and your new trait. To illustrate this, have a look at the basic implementation for `GSet` (reusing `GSetInterface` with another delta handling trait) and the reactive implementation of `EWFlag` (reusing `ReactiveCRDT` with another CRDT interface):

```scala
class GSet[E](
    val state: State[E],
    protected val antiEntropy: AntiEntropy[State[E]]
) extends GSetInterface[E, GSet[E]] with BasicCRDT[State[E], GSet[E]] {

  override protected def copy(state: State[E]): GSet[E] = new GSet(state, antiEntropy)
}

object GSet extends GSetCompanion {
  def apply[E](antiEntropy: AntiEntropy[State[E]]): GSet[E] = new GSet(UIJDLattice[State[E]].bottom, antiEntropy)
}
```

```scala
class EWFlag[C: CContext](
    val state: State[C],
    val replicaID: String,
    val deltaBuffer: List[Delta[State[C]]]
) extends EWFlagInterface[C, EWFlag[C]] with ReactiveCRDT[State[C], EWFlag[C]] {

  override protected def copy(state: State[C], deltaBuffer: List[Delta[State[C]]]): EWFlag[C] =
    new EWFlag(state, replicaID, deltaBuffer)
}

object EWFlag extends EWFlagCompanion {
  def apply[C: CContext](replicaID: String): EWFlag[C] =
    new EWFlag(UIJDLattice[State[C]].bottom, replicaID, List())
}
```

## Further Resources

For more information about the APIs of the CRDTs, you can refer to the scaladoc. If you want to have a closer look at the implementation details, check out our [GitHub repository](https://github.com/rescala-lang/REScala).

## References

[1] *Delta State Replicated Data Types.*<br>
P. Almeida, A. Shoker and C. Baquero.<br>
Journal of Parallel and Distributed Computing, Volume 111, pages 162-173, 2018.

[2] *Conflict-Free Replicated Data Types (CRDTs).*<br>
N. Preguiça, C. Baquero and M. Shapiro.<br>
arXiv preprint arXiv:1805.06358, 2018.

[3] *Efficient Synchronization of State-Based CRDTs.*<br>
V. Enes, P. S. Almeida, C. Baquero and J. Leitão.<br>
ICDE ’19, pages 148-159. IEEE, 2019.

[4] *The problem with embedded CRDT counters and a solution.*<br>
C. Baquero, P. Almeida and C. Lerche.<br>
PaPoC ’16, Article 10, pages 1-3. ACM, 2016.

[5] *Replicated abstract data types: Building blocks for collaborative applications.*<br>
H. Roh, M. Jeon, J. Kim and J. Lee.<br>
Journal of Parallel and Distributed Computing, Volume 71, pages 354-368, 2011.
