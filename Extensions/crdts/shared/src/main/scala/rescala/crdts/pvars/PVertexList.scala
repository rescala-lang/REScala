package rescala.crdts.pvars

import rescala.crdts.pvars.DistributedSignal.PVarFactory
import rescala.crdts.statecrdts.StateCRDT
import rescala.crdts.statecrdts.sequences.{RGA, ValueVertex, Vertex}
import rescala.default._

/**
  * DistributedVertexLists are LinkedLists operating on so called Vertices. Vertices store a value of type `A`.
  *
  * @param initial The initial value of this variable.
  */
case class PVertexList[A](initial: RGA[A] = RGA.empty[A])
extends DistributedSignal[List[A], RGA[A]](initial) {
  def contains(v: Vertex[A]): Boolean = crdtSignal.readValueOnce.contains(v)


  /**
    *
    * @return True if the list contains both u and v and u is ordered before v.
    */
  def before(u: Vertex[A], v: Vertex[A]): Boolean = crdtSignal.readValueOnce.before(u, v)

  /**
    * To store values in a DistributedVertexList one has to wrap the value in a Vertex by writing Vertex(value). The
    * insertion position is specified by passing the vertex left of the new vertex.
    *
    * @param position the vertex left of the new vertex
    * @param vertex   the vertex to be inserted
    */
  def addRight(position: Vertex[A], vertex: ValueVertex[A]): Unit = localDeviceChange.fire(crdtSignal.readValueOnce.addRight(position, vertex))

  def append(vertex: ValueVertex[A]): Unit = localDeviceChange.fire(crdtSignal.readValueOnce.append(vertex))

  def successor(v: Vertex[A]): Vertex[A] = crdtSignal.readValueOnce.successor(v)

  def valueIterator: Iterator[A] = crdtSignal.readValueOnce.iterator

  def iterator: Iterator[Vertex[A]] = crdtSignal.readValueOnce.vertexIterator
}

object PVertexList {
  /**
    * Allows creation of DistributedVertexLists by passing a list of initial values.
    */
  def apply[A](values: List[A]): PVertexList[A] = {
    val init: RGA[A] = implicitly[StateCRDT[List[A], RGA[A]]].fromValue(values)
    new PVertexList[A](init)
  }

  //noinspection ConvertExpressionToSAM
  implicit def PVertexListFactory[A]: PVarFactory[PVertexList[A]] =
    new PVarFactory[PVertexList[A]] {
      override def apply(): PVertexList[A] = PVertexList[A]()
    }
}
