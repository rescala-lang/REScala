package rescala.distributables

import rescala.default._
import rescala.distributables.DistributedSignal.PVarFactory
import rescala.lattices.sequences.RGA.RGA
import rescala.lattices.sequences.{RGA, Vertex}

/**
  * DistributedVertexLists are LinkedLists operating on so called Vertices. Vertices store a value of type `A`.
  *
  * @param initial The initial value of this variable.
  */
case class PVertexList[A](initial: RGA[A] = RGA.empty[A])
extends DistributedSignal[List[A], RGA[A]](initial, _.value) {
  def contains(v: Vertex): Boolean = crdtSignal.readValueOnce.contains(v)


  /**
    *
    * @return True if the list contains both u and v and u is ordered before v.
    */
  def before(u: Vertex, v: Vertex): Boolean = crdtSignal.readValueOnce.before(u, v)

  def append(value: A): Unit = crdtSignal.transform(_.append(value))

  def successor(v: Vertex): Vertex = crdtSignal.readValueOnce.successor(v)

  def valueIterator: Iterator[A] = crdtSignal.readValueOnce.iterator

  def iterator: Iterator[Vertex] = crdtSignal.readValueOnce.vertexIterator
}

object PVertexList {
  /**
    * Allows creation of DistributedVertexLists by passing a list of initial values.
    */
  def apply[A](values: List[A]): PVertexList[A] = {
    val init: RGA[A] = RGA[A](values)
    new PVertexList[A](init)
  }

  //noinspection ConvertExpressionToSAM
  implicit def PVertexListFactory[A]: PVarFactory[PVertexList[A]] =
    new PVarFactory[PVertexList[A]] {
      override def apply(): PVertexList[A] = PVertexList[A]()
    }
}
