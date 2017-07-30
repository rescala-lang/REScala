package distributionengine

import akka.actor._
import rescala._
import statecrdts._

/**
  * DistributedGCounters are increase-only counter variables.
  *
  * @param engine  The DistributionEngine used to handle distribution.
  * @param name    The name of this variable. Has to be the same on all hosts to be synchronized!
  * @param initial The initial value of this variable.
  */
case class DistributedGCounter(engine: ActorRef, name: String, initial: CIncOnlyCounter = CIncOnlyCounter(0),
                               internalChanges: rescala.Evt[CIncOnlyCounter] = Evt[CIncOnlyCounter],
                               externalChanges: rescala.Evt[CIncOnlyCounter] = Evt[CIncOnlyCounter])
  extends Publishable[CIncOnlyCounter] {

  def increase: Int = {
    internalChanges(signal.now.increase)
    getValue
  }
}

object DistributedGCounter {
  /**
    * Allows creation of DistributedGCounters by passing a start value.
    */
  def apply(engine: ActorRef, name: String, start: Int): DistributedGCounter = {
    val init: CIncOnlyCounter = CIncOnlyCounter(start)
    new DistributedGCounter(engine, name, init)
  }
}

/**
  * DistributedVertexLists are LinkedLists operating on so called Vertices. Vertices store a value of type `A`.
  *
  * @param engine  The DistributionEngine used to handle distribution.
  * @param name    The name of this variable. Has to be the same on all hosts to be synchronized!
  * @param initial The initial value of this variable.
  */
case class DistributedVertexList[A](engine: ActorRef, name: String, initial: RGA[A] = RGA(),
                                    internalChanges: Evt[RGA[A]] = Evt[RGA[A]],
                                    externalChanges: Evt[RGA[A]] = Evt[RGA[A]]) extends Publishable[RGA[A]] {
  def contains[A1 >: A](v: Vertex[A1]): Boolean = signal.now.contains(v)

  def before[A1 >: A](u: Vertex[A1], v: Vertex[A1]): Boolean = signal.now.before(u, v)

  /**
    * To store values in a DistributedVertexList one has to wrap the value in a Vertex by writing Vertex(value). The
    * insertion position is specified by passing the vertex left of the new vertex.
    *
    * @param position the vertex left of the new vertex
    * @param vertex   the vertex to be inserted
    */
  def addRight[A1 >: A](position: Vertex[A1], vertex: Vertex[A]): Unit = internalChanges(signal.now.addRight(position, vertex))

  def successor[A1 >: A](v: Vertex[A1]): Vertex[A1] = signal.now.successor(v)

  /**
    * Converts this DistributedVertexList into a list of type `A`.
    *
    * @return A list containing all values of the vertices in this vertex list in order.
    */
  def toList: List[A] = getValue
}

object DistributedVertexList {
  /**
    * Allows creation of DistributedVertexLists by passing a list of initial values.
    */
  def apply[A](engine: ActorRef, name: String, values: List[A]): DistributedVertexList[A] = {
    val init: RGA[A] = RGA.empty[A].fromValue(values)
    new DistributedVertexList[A](engine, name, init)
  }
}

case class DistributedSet[A](engine: ActorRef, name: String, initial: ORSet[A] = ORSet(),
                             internalChanges: Evt[ORSet[A]] = Evt[ORSet[A]],
                             externalChanges: Evt[ORSet[A]] = Evt[ORSet[A]]) extends Publishable[ORSet[A]] {

  def add(a: A): Unit = internalChanges(signal.now.add(a))

  def remove(a: A): Unit = internalChanges(signal.now.remove(a))

  def contains(a: A): Boolean = signal.now.contains(a)
}

object DistributedSet {
  /**
    * Allows creation of DistributedSets by passing a set of initial values.
    */
  def apply[A](engine: ActorRef, name: String, values: Set[A]): DistributedSet[A] = {
    val init: ORSet[A] = ORSet().fromValue(values)
    new DistributedSet[A](engine, name, init)
  }
}