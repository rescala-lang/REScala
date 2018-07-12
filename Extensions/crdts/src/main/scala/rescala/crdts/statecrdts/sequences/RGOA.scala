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
case class RGOA[A](payload: (GSet[ValueVertex[A]], HashMap[Vertex, Vertex])) extends CRDTSequence[A] {
  override type selfType = RGOA[A]
  override type valueType = List[A]
  override type payloadType = (GSet[ValueVertex[A]], HashMap[Vertex, Vertex])

  val logger: Logger = Logger[RGOA[A]]
  val (vertices, edges): (GSet[ValueVertex[A]], HashMap[Vertex, Vertex]) = payload
}

object RGOA {
  def apply[A](values: A*): RGOA[A] = {
    val r = RGOA[A]()
    r.fromValue(values.toList)
  }

  def apply[A](): RGOA[A] = empty

  def empty[A]: RGOA[A] = new RGOA[A]((GSet(Vertex.start, Vertex.end), HashMap(Vertex.start -> Vertex.end)))

  implicit def RGOAStateCRDTInstance[A]: StateCRDT[List[A], RGOA[A]] = new StateCRDT[List[A], RGOA[A]] {
    override def value(target: RGOA[A]): List[A] = target.value

    override def merge(left: RGOA[A], r: RGOA[A]): RGOA[A] = {
      val newVertices = r.vertexIterator.toList.filter(!left.vertices.contains(_))

      left.logger.debug(s"Merging $r into $left")
      left.logger.debug(s"found new vertices: $newVertices")

      // build map of old insertion positions of the new vertices
      val oldPositions = r.edges.foldLeft(Map(): Map[Vertex[Any], Vertex[Any]]) {
        case (m, (u, v)) => if (newVertices.contains(v)) m + (v -> u) else m
      }

      // update edges by inserting vertices at the right positions
      newVertices.foldLeft(left) {
        case (merged: RGOA[A], v: Vertex[A]) =>
          left.logger.debug(s"inserting $v at position ${oldPositions(v)}")
          merged.insert(oldPositions(v), v)
      }
    }
  }
}
