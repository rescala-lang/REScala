package rescala.crdts.statecrdts
package sequences

import rescala.crdts.statecrdts.sets.GrowOnlySet

import scala.collection.immutable.HashMap

/**
  * Replicated Grow Only Array - A modified RGA version using grow only sets for vertices
  *
  * @param payload The payload consist of a Grow-Only Set which stores the vertices and one HashMap which stores the
  *                edges between vertices.
  * @tparam A The type of the elements stored in this array.
  */
case class RGOA[A](payload: (GrowOnlySet[ValueVertex[A]], Map[Vertex[A], Vertex[A]])) extends CRDTSequence[A] {
  override type SelfT = RGOA[A]
  override type PayloadT = (GrowOnlySet[ValueVertex[A]], Map[Vertex[A], Vertex[A]])
  val (vertices, edges): (GrowOnlySet[ValueVertex[A]], Map[Vertex[A], Vertex[A]]) = payload

  override def fromPayload(payload: (GrowOnlySet[ValueVertex[A]], Map[Vertex[A], Vertex[A]])): RGOA[A] = RGOA[A](payload)
}

object RGOA {
  def apply[A](values: A*): RGOA[A] = {
    apply(values.toList)
  }

  def apply[A](): RGOA[A] = empty

  def empty[A]: RGOA[A] = new RGOA[A]((GrowOnlySet[ValueVertex[A]](), HashMap[Vertex[A], Vertex[A]](StartVertex -> EndVertex)))


  /** Allows the creation of new CRDTs by passing an initial value.
    *
    * @param value the value
    * @return new CRDT instance representing the value
    */
  def apply[A](value: List[A]): RGOA[A] = {
    val emptyPayload: (GrowOnlySet[ValueVertex[A]], HashMap[Vertex[A], Vertex[A]]) =
      (GrowOnlySet[ValueVertex[A]](), HashMap[Vertex[A], Vertex[A]](StartVertex -> EndVertex))
    val newRGOA: RGOA[A] = RGOA[A](emptyPayload)

    value.reverse.foldLeft(newRGOA) {
      case (r: RGOA[A], a) => r.addRight(StartVertex, a)
    }
  }


  implicit def RGOAStateCRDTInstance[A]: StateCRDT[List[A], RGOA[A]] = new StateCRDT[List[A], RGOA[A]] {
    override def value(target: RGOA[A]): List[A] = target.value

    override def merge(left: RGOA[A], right: RGOA[A]): RGOA[A] = {
      println(s"Merging $right into $left")

      val newVertices = right.vertexIterator.toList.filter(!left.edges.contains(_))
      println(s"found new vertices in right: $newVertices")

      // build map of old insertion positions of the new vertices
      val oldPositions = right.edges.foldLeft(Map(): Map[Vertex[A], Vertex[A]]) {
        case (m, (u, v)) => if (newVertices.contains(v)) m + (v -> u) else m
      }

      // update edges by inserting vertices at the right positions
      newVertices.foldLeft(left) {
        case (merged: RGOA[A], v: Vertex[A]) =>
          println(s"inserting $v at position ${oldPositions(v)}")
          merged.addRight(oldPositions(v), v)
      }
    }

  }
}
