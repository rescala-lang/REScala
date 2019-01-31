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
case class RGA[A](payload: (TwoPSet[ValueVertex[A]], HashMap[Vertex[A], Vertex[A]])) extends RemovableCRDTSequence[A] {
  override type selfType = RGA[A]
  override type payloadType = (TwoPSet[ValueVertex[A]], HashMap[Vertex[A], Vertex[A]])

  val (vertices, edges): (TwoPSet[ValueVertex[A]], HashMap[Vertex[A], Vertex[A]]) = payload

  override def fromPayload(payload: (TwoPSet[ValueVertex[A]], HashMap[Vertex[A], Vertex[A]])): RGA[A] = RGA[A](payload)
}


object RGA {
  def apply[A](values: A*): RGA[A] = RGA2CRDTInstance.fromValue(values.toList)

  def apply[A](): RGA[A] = empty

  def empty[A]: RGA[A] = new RGA[A]((TwoPSet[ValueVertex[A]](), HashMap(startVertex -> endVertex)))

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

      def fromValue(value: List[A]): RGA[A] = {
        val emptyPayload: (TwoPSet[Vertex[A]], HashMap[Vertex[A], Vertex[A]]) =
          (TwoPSet[Vertex[A]](), HashMap[Vertex[A], Vertex[A]](startVertex -> endVertex))
        val newRGA: RGA[A] = fromPayload(emptyPayload)

        value.reverse.foldLeft(newRGA) {
          case (r: RGA[A], a) => r.addRight(startVertex, a)
        }
      }

      //override def fromPayload[((TwoPSet[Vertex[A]], HashMap[Vertex, Vertex]))](payload: (TwoPSet[Vertex[A]], HashMap[Vertex, Vertex])): RGA[A] = RGA(payload)
      /** Allows the creation of new CRDTs by passing a payload.
        *
        * @param payload the payload
        * @return new CRDT instance with the given payload
        */
      override def fromPayload[P](payload: P): RGA[A] = RGA(payload
        .asInstanceOf[(TwoPSet[ValueVertex[A]], HashMap[Vertex[A], Vertex[A]])])
    }
}

