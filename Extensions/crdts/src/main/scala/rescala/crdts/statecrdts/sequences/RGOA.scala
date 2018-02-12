package rescala.crdts.statecrdts
package sequences

import com.typesafe.scalalogging.Logger
import rescala.crdts.statecrdts.sets.GSet

import scala.collection.AbstractIterator
import scala.collection.immutable.HashMap

// TODO: Refactor RGA classes to make proper use of the traits. The current version includes lots of duplicate code.
/**
  * Replicated Grow Only Array - A modified RGA version using grow only sets for vertices
  *
  * @param payload The payload consist of one TwoPhase Set which stores the vertices, one HashMap which stores the edges
  *                between vertices and one HashMap which stores a Timestamp for each Vertex.
  * @tparam A The type of the elements stored in this array
  */
case class RGOA[A](payload: (GSet[Vertex[Any]], HashMap[Vertex[Any], Vertex[Any]])) extends StateCRDTSequence[A] {
  type selfType = RGOA[A]
  type valueType = List[A]
  type payloadType = (GSet[Vertex[Any]], HashMap[Vertex[Any], Vertex[Any]])
  val logger: Logger = Logger[RGOA[A]]
  val (_, edges): ((GSet[Vertex[Any]], HashMap[Vertex[Any], Vertex[Any]])) = payload


  override def vertices: GSet[Vertex[Any]] = payload._1

  override def before[A1 >: A](u: Vertex[A1], v: Vertex[A1]): Boolean = u match {
    case Vertex.end => false
    case Vertex.start => true
    case _ => edges(u) == v || before(edges(u), v)
  }

  def successor[A1 >: A](v: Vertex[A1]): Vertex[A1] = v match {
    case Vertex.end =>
      logger.warn("There is no successor to the end node! ")
      v
    case _ =>
      val u: Vertex[A1] = edges(v).asInstanceOf[Vertex[A1]]
      if (vertices.contains(u)) u
      else successor(u)
  }

  def addRight[A1 >: A](position: Vertex[A1], a: A): RGOA[A] = addRight(position, new Vertex(a, Vertex.genTimestamp))

  def addRight[A1 >: A](position: Vertex[A1], v: Vertex[A]): RGOA[A] = insert(position, v)

  def append(a: A): RGOA[A] = {
    append(Vertex(a))
  }

  def append(v: Vertex[A]): RGOA[A] = {
    val position = if (vertexIterator.nonEmpty) vertexIterator.toList.last else Vertex.start
    insert(position, v)
  }

  def prepend(v: Vertex[A]): RGOA[A] = {
    insert(Vertex.start, v)
  }

  def value: List[A] = iterator.toList

  def iterator: Iterator[A] = vertexIterator.map(v => v.value)

  def vertexIterator: Iterator[Vertex[A]] = new AbstractIterator[Vertex[A]] {
    var lastVertex: Vertex[Any] = Vertex.start

    override def hasNext: Boolean = successor(lastVertex) match {
      case Vertex.end => false
      case _ => true
    }

    override def next(): Vertex[A] = {
      successor(lastVertex) match {
        case v: Vertex[A] => lastVertex = v; v
      }
    }
  }

  def fromPayload(payload: payloadType): RGOA[A] = RGOA(payload)

  def fromValue(value: valueType): RGOA[A] = {
    val emptyPayload: payloadType = (GSet[Vertex[Any]](Vertex.start, Vertex.end), HashMap[Vertex[Any], Vertex[Any]](Vertex.start -> Vertex.end))
    val newRGA: RGOA[A] = fromPayload(emptyPayload)

    value.reverse.foldLeft(newRGA) {
      case (r: RGOA[A], a) => r.addRight(Vertex.start, a)
    }
  }

  /**
    * This method allows insertions of any type into the RAG. This is used to move the start and end nodes
    *
    * @param position the vertex specifying the position
    * @param v        the vertex to be inserted right to position
    * @tparam A1 type of both the inserted vertex and the vertex specifying the position
    * @return A new RAG containing the inserted element
    */
  private def insert[A1 >: A](position: Vertex[A1], v: Vertex[A1]): RGOA[A] = position match {
    case Vertex.end =>
      logger.error("Cannot insert after end node!")
      this
    case _ => if (vertices.contains(position)) {
      val (l, r) = (position, successor(position))
      // Check if the vertex right to us has been inserted after us.  If yes, insert v after the new vertex.
      if (r.timestamp > v.timestamp) insert(r, v)
      else {
        val newVertices = vertices.add(v)
        val newEdges = edges + (l -> v) + (v -> r)
        new RGOA((newVertices, newEdges))
      }
    }
    else {
      logger.error(s"Insertion failed! RGA does not contain specified position vertex $position!")
      this
    }
  }
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
