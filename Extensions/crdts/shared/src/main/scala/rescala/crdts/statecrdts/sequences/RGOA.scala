package rescala.crdts.statecrdts
package sequences

import com.typesafe.scalalogging.Logger
import rescala.crdts.statecrdts.sets.GSet

import scala.collection.immutable.HashMap

/**
  * Replicated Grow Only Array - A modified RGA version using grow only sets for vertices
  *
  * @param payload The payload consist of a Grow-Only Set which stores the vertices and one HashMap which stores the
  *                edges between vertices.
  * @tparam A The type of the elements stored in this array.
  */
case class RGOA[A](payload: (GSet[Vertex[A]], HashMap[Vertex[A], Vertex[A]])) extends CRDTSequence[A] {
  override type selfType = RGOA[A]
  override type payloadType = (GSet[Vertex[A]], HashMap[Vertex[A], Vertex[A]])
  val logger: Logger = Logger[RGOA[A]]
  val (vertices, edges): (GSet[Vertex[A]], HashMap[Vertex[A], Vertex[A]]) = payload

  override def fromPayload(payload: (GSet[Vertex[A]], HashMap[Vertex[A], Vertex[A]])): RGOA[A] = RGOA[A](payload)
}

object RGOA {
  def apply[A](values: A*): RGOA[A] = {
    RGOAStateCRDTInstance.fromValue(values.toList)
  }

  def apply[A](): RGOA[A] = empty

  def empty[A]: RGOA[A] = new RGOA[A]((GSet[Vertex[A]](), HashMap[Vertex[A], Vertex[A]](Vertex.start -> Vertex.end)))

  implicit def RGOAStateCRDTInstance[A]: StateCRDT[List[A], RGOA[A]] = new StateCRDT[List[A], RGOA[A]] {
    override def value(target: RGOA[A]): List[A] = target.value

    override def merge(left: RGOA[A], r: RGOA[A]): RGOA[A] = {
      val newVertices = r.vertexIterator.toList.filter(!left.vertices.contains(_))

      left.logger.debug(s"Merging $r into $left")
      left.logger.debug(s"found new vertices: $newVertices")

      // build map of old insertion positions of the new vertices
      val oldPositions = r.edges.foldLeft(Map(): Map[Vertex[A], Vertex[A]]) {
        case (m, (u, v)) => if (newVertices.contains(v)) m + (v -> u) else m
      }

      // update edges by inserting vertices at the right positions
      newVertices.foldLeft(left) {
        case (merged: RGOA[A], v: Vertex[A]) =>
          left.logger.debug(s"inserting $v at position ${oldPositions(v)}")
          merged.addRight(oldPositions(v), v)
      }
    }

    /** Allows the creation of new CRDTs by passing an initial value.
      *
      * @param value the value
      * @return new CRDT instance representing the value
      */
    override def fromValue(value: List[A]): RGOA[A] = {
      val emptyPayload: (GSet[Vertex[A]], HashMap[Vertex[A], Vertex[A]]) =
        (GSet[Vertex[A]](), HashMap[Vertex[A], Vertex[A]](Vertex.start -> Vertex.end))
      val newRGOA: RGOA[A] = fromPayload(emptyPayload)

      value.reverse.foldLeft(newRGOA) {
        case (r: RGOA[A], a) => r.addRight(Vertex.start, a)
      }
    }

    /** Allows the creation of new CRDTs by passing a payload.
      *
      * @param payload the payload
      * @return new CRDT instance with the given payload
      */
    override def fromPayload[P](payload: P): RGOA[A] = RGOA(payload.
      asInstanceOf[(GSet[Vertex[A]], HashMap[Vertex[A], Vertex[A]])])
  }
}
