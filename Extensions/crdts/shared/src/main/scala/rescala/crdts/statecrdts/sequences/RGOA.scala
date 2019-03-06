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
case class RGOA[A](vertices: GrowOnlySet[Vertex],
                   edges: Map[Vertex, Vertex],
                   values: Map[Vertex, A])
  extends CRDTSequence[A] {
  override type SelfT = RGOA[A]
  override type PayloadT = (GrowOnlySet[Vertex], Map[Vertex, Vertex], Map[Vertex, A])

  override def fromPayload(payload: PayloadT): RGOA[A] =
    RGOA[A](payload._1, payload._2, payload._3)
}

object RGOA {

  def empty[A]: RGOA[A] = new RGOA[A](GrowOnlySet[Vertex](), HashMap[Vertex, Vertex](Vertex.start -> Vertex.end), Map())


  /** Allows the creation of new CRDTs by passing an initial value.
    *
    * @param value the value
    * @return new CRDT instance representing the value
    */
  def apply[A](value: Seq[A]): RGOA[A] = {
    value.reverse.foldLeft(empty[A]) {
      case (r: RGOA[A], a) => r.addRight(Vertex.start, a)
    }
  }


  implicit def crdt[A]: StateCRDT[List[A], RGOA[A]] = new StateCRDT[List[A], RGOA[A]] {
    override def value(target: RGOA[A]): List[A] = target.value

    override def merge(left: RGOA[A], right: RGOA[A]): RGOA[A] = {
//      println(s"Merging $right into $left")

      val newVertices = right.vertexIterator.toList.filter(!left.edges.contains(_))
//      println(s"found new vertices in right: $newVertices")

      // build map of old insertion positions of the new vertices
      val oldPositions = right.edges.foldLeft(Map(): Map[Vertex, Vertex]) {
        case (m, (u, v)) => if (newVertices.contains(v)) m + (v -> u) else m
      }

      // update edges by inserting vertices at the right positions
      newVertices.foldLeft(left) {
        case (merged: RGOA[A], v: Vertex) =>
          println(s"inserting $v at position ${oldPositions(v)}")
          merged.addRight(oldPositions(v), v, left.values(v))
      }
    }

  }
}
