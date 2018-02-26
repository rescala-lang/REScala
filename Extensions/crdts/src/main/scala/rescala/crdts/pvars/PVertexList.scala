package rescala.crdts.pvars

import rescala._
import rescala.crdts.statecrdts.sequences.{RGA, Vertex}

/**
  * DistributedVertexLists are LinkedLists operating on so called Vertices. Vertices store a value of type `A`.
  *
  * @param initial The initial value of this variable.
  */
case class PVertexList[A](initial: RGA[A] = RGA.empty[A],
                          internalChanges: Evt[RGA[A]] = Evt[RGA[A]],
                          externalChanges: Evt[RGA[A]] = Evt[RGA[A]]) extends Publishable[List[A], RGA[A]] {
  def contains[A1 >: A](v: Vertex[A1]): Boolean = crdtSignal.readValueOnce.contains(v)


  /**
    *
    * @return True if the list contains both u and v and u is ordered before v.
    */
  def before[A1 >: A](u: Vertex[A1], v: Vertex[A1]): Boolean = crdtSignal.readValueOnce.before(u, v)

  /**
    * To store values in a DistributedVertexList one has to wrap the value in a Vertex by writing Vertex(value). The
    * insertion position is specified by passing the vertex left of the new vertex.
    *
    * @param position the vertex left of the new vertex
    * @param vertex   the vertex to be inserted
    */
  def addRight[A1 >: A](position: Vertex[A1], vertex: Vertex[A]): Unit = internalChanges.fire(crdtSignal.readValueOnce.addRight(position, vertex))

  def append(vertex: Vertex[A]): Unit = internalChanges.fire(crdtSignal.readValueOnce.append(vertex))

  def successor[A1 >: A](v: Vertex[A1]): Vertex[A1] = crdtSignal.readValueOnce.successor(v)

  def valueIterator: Iterator[A] = crdtSignal.readValueOnce.iterator

  def iterator: Iterator[Vertex[A]] = crdtSignal.readValueOnce.vertexIterator
}

object PVertexList {
  /**
    * Allows creation of DistributedVertexLists by passing a list of initial values.
    */
  def apply[A](values: List[A]): PVertexList[A] = {
    val init: RGA[A] = RGA.empty[A].fromValue(values)
    new PVertexList[A](init)
  }
}
