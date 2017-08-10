package distributionengine

import rescala._
import statecrdts._

/**
  * DistributedGCounters are increase-only counter variables.
  *
  * @param name    The name of this variable. Has to be the same on all hosts to be synchronized!
  * @param initial The initial value of this variable.
  */
case class PGCounter(name: String, initial: CIncOnlyCounter = CIncOnlyCounter(0),
                     internalChanges: rescala.Evt[CIncOnlyCounter] = Evt[CIncOnlyCounter],
                     externalChanges: rescala.Evt[CIncOnlyCounter] = Evt[CIncOnlyCounter])
  extends Publishable[CIncOnlyCounter] {

  def increase: Int = {
    internalChanges(crdtSignal.now.increase)
    value
  }
}

object PGCounter {
  /**
    * Allows creation of DistributedGCounters by passing a start value.
    */
  def apply(name: String, start: Int): PGCounter = {
    val init: CIncOnlyCounter = CIncOnlyCounter(start)
    new PGCounter(name, init)
  }
}

/**
  * DistributedVertexLists are LinkedLists operating on so called Vertices. Vertices store a value of type `A`.
  *
  * @param name    The name of this variable. Has to be the same on all hosts to be synchronized!
  * @param initial The initial value of this variable.
  */
case class PVertexList[A](name: String, initial: RGA[A] = RGA.empty[A],
                          internalChanges: Evt[RGA[A]] = Evt[RGA[A]],
                          externalChanges: Evt[RGA[A]] = Evt[RGA[A]]) extends Publishable[RGA[A]] {
  def contains[A1 >: A](v: Vertex[A1]): Boolean = crdtSignal.now.contains(v)


  /**
    *
    * @return True if the list contains both u and v and u is ordered before v.
    */
  def before[A1 >: A](u: Vertex[A1], v: Vertex[A1]): Boolean = crdtSignal.now.before(u, v)

  /**
    * To store values in a DistributedVertexList one has to wrap the value in a Vertex by writing Vertex(value). The
    * insertion position is specified by passing the vertex left of the new vertex.
    *
    * @param position the vertex left of the new vertex
    * @param vertex   the vertex to be inserted
    */
  def addRight[A1 >: A](position: Vertex[A1], vertex: Vertex[A]): Unit = internalChanges(crdtSignal.now.addRight(position, vertex))

  def append(vertex: Vertex[A]): Unit = internalChanges(crdtSignal.now.append(vertex))

  def successor[A1 >: A](v: Vertex[A1]): Vertex[A1] = crdtSignal.now.successor(v)

  def valueIterator: Iterator[A] = crdtSignal.now.iterator

  def iterator: Iterator[Vertex[A]] = crdtSignal.now.vertexIterator
}

object PVertexList {
  /**
    * Allows creation of DistributedVertexLists by passing a list of initial values.
    */
  def apply[A](name: String, values: List[A]): PVertexList[A] = {
    val init: RGA[A] = RGA.empty[A].fromValue(values)
    new PVertexList[A](name, init)
  }
}

case class DistributedSet[A](name: String, initial: ORSet[A] = ORSet(),
                             internalChanges: Evt[ORSet[A]] = Evt[ORSet[A]],
                             externalChanges: Evt[ORSet[A]] = Evt[ORSet[A]]) extends Publishable[ORSet[A]] {

  def add(a: A): Unit = internalChanges(crdtSignal.now.add(a))

  def remove(a: A): Unit = internalChanges(crdtSignal.now.remove(a))

  def contains(a: A): Boolean = crdtSignal.now.contains(a)
}

object DistributedSet {
  /**
    * Allows creation of DistributedSets by passing a set of initial values.
    */
  def apply[A](name: String, values: Set[A]): DistributedSet[A] = {
    val init: ORSet[A] = ORSet().fromValue(values)
    new DistributedSet[A](name, init)
  }
}

case class PVar[A <: StateCRDT](name: String, initial: A) extends Publishable[A] {
  override val internalChanges: Evt[A] = Evt[A]
  override val externalChanges: Evt[A] = Evt[A]

  def apply(a: A): Unit = set(a)

  def set(a: A): Unit = internalChanges(a)
}