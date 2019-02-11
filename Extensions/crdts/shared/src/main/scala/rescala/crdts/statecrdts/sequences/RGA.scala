package rescala.crdts.statecrdts.sequences

import rescala.crdts.statecrdts.StateCRDT
import rescala.crdts.statecrdts.sets.TwoPSet

import scala.collection.immutable.HashMap

/**
  * Replicated Growable Array
  *
  * @param payload The payload consist of one TwoPhase Set which stores the vertices, one HashMap which stores the edges
  *                between vertices.
  * @tparam A The type of the elements stored in this array
  */
case class RGA[A](payload: RGA.Payload[A]) extends CRDTSequence[A] {
  override type SelfT = RGA[A]
  override type PayloadT = (TwoPSet[ValueVertex[A]], HashMap[Vertex[A], Vertex[A]])

  val (vertices, edges): (TwoPSet[ValueVertex[A]], HashMap[Vertex[A], Vertex[A]]) = payload

  override def fromPayload(payload: PayloadT): RGA[A] = RGA[A](payload)

  def remove(v: ValueVertex[A]): SelfT = RGA((vertices.remove(v), edges))

}


object RGA {
  type Payload[A] = (TwoPSet[ValueVertex[A]], HashMap[Vertex[A], Vertex[A]])

  def apply[A](values: A*): RGA[A] = {
    val emptyPayload: (TwoPSet[ValueVertex[A]], HashMap[Vertex[A], Vertex[A]]) =
      (TwoPSet[ValueVertex[A]](), HashMap[Vertex[A], Vertex[A]](StartVertex -> EndVertex))
    val newRGA: RGA[A] = apply[A](emptyPayload)

    values.reverse.foldLeft(newRGA) {
      case (r: RGA[A], a) => r.addRight(StartVertex, a)
    }
  }

  def apply[A](value: List[A]): RGA[A] = apply(value : _*)

  def apply[A](): RGA[A] = empty

  def empty[A]: RGA[A] = new RGA[A]((TwoPSet[ValueVertex[A]](), HashMap(StartVertex -> EndVertex)))


  implicit def RGA2CRDTInstance[A]: StateCRDT[List[A], RGA[A]] =
    new StateCRDT[List[A], RGA[A]] {
      override def value(target: RGA[A]): List[A] = target.value

      override def merge(left: RGA[A], r: RGA[A]): RGA[A] = {
        val newVertices = r.vertexIterator.toList.filter(!left.edges.contains(_))

//        left.logger.debug(s"Merging $r into $left")
//        left.logger.debug(s"found new vertices: $newVertices")

        // build map of old insertion positions of the new vertices
        val oldPositions = r.edges.foldLeft(Map(): Map[Vertex[A], Vertex[A]]) {
          case (m, (u, v)) => if (newVertices.contains(v)) m + (v -> u) else m
        }

        // update edges by inserting vertices at the right positions
        val mergedEdges = newVertices.foldLeft(left) {
          case (merged: RGA[A], v: Vertex[A]) =>
//            left.logger.debug(s"inserting $v at position ${oldPositions(v)}")
            merged.addRight(oldPositions(v), v)
        }.edges

        // merge vertices
        val mergedVertices = TwoPSet.TwoPSetCRDTInstance.merge(left.vertices, r.vertices)

        RGA((mergedVertices, mergedEdges))
      }
    }
}

