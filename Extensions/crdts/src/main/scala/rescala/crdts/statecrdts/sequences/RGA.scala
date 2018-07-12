package rescala.crdts.statecrdts.sequences

import com.typesafe.scalalogging.Logger
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
case class RGA[A](payload: (TwoPSet[ValueVertex[A]], HashMap[Vertex, Vertex])) extends RemovableCRDTSequence[A] {
  override type payloadType = (TwoPSet[ValueVertex[A]], HashMap[Vertex, Vertex])
  override type selfType = RGA[A]

  val (vertices, edges): (TwoPSet[ValueVertex[A]], HashMap[Vertex, Vertex]) = payload
  val logger: Logger = Logger[RGA[A]]
}


object RGA {
  def apply[A](values: A*): RGA[A] = RGA2CRDTInstance.fromValue(values.toList)

  def apply[A](): RGA[A] = empty

  def empty[A]: RGA[A] = new RGA[A]((TwoPSet[ValueVertex[A]](), HashMap(Vertex.start -> Vertex.end)))

  implicit def RGA2CRDTInstance[A]: StateCRDT[List[A], RGA[A]] = new StateCRDT[List[A], RGA[A]] {
    override def value(target: RGA[A]): List[A] = target.value

    override def merge(left: RGA[A], r: RGA[A]): RGA[A] = {
      val newVertices = r.vertexIterator.toList.filter(!left.vertices.contains(_))

      left.logger.debug(s"Merging $r into $left")
      left.logger.debug(s"found new vertices: $newVertices")

      // build map of old insertion positions of the new vertices
      val oldPositions = r.edges.foldLeft(Map(): Map[Vertex, Vertex]) {
        case (m, (u, v)) => if (newVertices.contains(v)) m + (v -> u) else m
      }

      // update edges by inserting vertices at the right positions
      val mergedEdges = newVertices.foldLeft(left) {
        case (merged: RGA[A], v: Vertex) =>
          left.logger.debug(s"inserting $v at position ${oldPositions(v)}")
          merged.addRight(oldPositions(v), v)
      }.edges

      // merge vertices
      val mergedVertices = TwoPSet.TwoPSetCRDTInstance.merge(left.vertices, r.vertices)

      RGA((mergedVertices, mergedEdges))
    }

    def fromValue(value: List[A]): RGA[A] = {
      val emptyPayload: (TwoPSet[ValueVertex[A]], HashMap[Vertex, Vertex]) =
        (TwoPSet[ValueVertex[A]](), HashMap[Vertex, Vertex](Vertex.start -> Vertex.end))
      val newRGA: RGA[A] = fromPayload(emptyPayload)

      value.reverse.foldLeft(newRGA) {
        case (r: RGA[A], a) => r.addRight(Vertex.start, a)
      }
    }

    override def fromPayload[P <: (TwoPSet[ValueVertex[A]], HashMap[Vertex, Vertex])](payload: P): RGA[A] = RGA(payload)
  }
}

