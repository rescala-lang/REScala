package rescala.crdts.statecrdts.sequences

import com.typesafe.scalalogging.Logger
import rescala.crdts.statecrdts.StateCRDT
import rescala.crdts.statecrdts.sets.TwoPSet

import scala.collection.AbstractIterator
import scala.collection.immutable.HashMap

/**
  * Replicated Growable Array
  *
  * @param payload The payload consist of one TwoPhase Set which stores the vertices, one HashMap which stores the edges
  *                between vertices and one HashMap which stores a Timestamp for each Vertex.
  * @tparam A The type of the elements stored in this array
  */
case class RGA[A](payload: (TwoPSet[ValueVertex[A]], HashMap[Vertex, Vertex])) extends StateCRDTSequence[A] with RemovableSequence[A] {
  override type selfType = RGA[A]
  type valueType = List[A]
  type payloadType = (TwoPSet[ValueVertex[A]], HashMap[Vertex, Vertex])
  val logger: Logger = Logger[RGA[A]]
  val (_, edges): (TwoPSet[ValueVertex[A]], HashMap[Vertex, Vertex]) = payload

  override def before(u: Vertex, v: Vertex): Boolean = u match {
    case `startVertex` => true
    case `endVertex` => false
    case u: ValueVertex[A] => edges(u) == v || before(edges(u), v)
    case _ => throw new IllegalArgumentException(s"Sequence does not contain Vertex $u!")
  }

  def successor[A1 >: A](v: Vertex): Vertex = v match {
    case `endVertex` =>
      throw new IllegalArgumentException("There is no successor to the end node!")
    case _ => {
      if (edges.contains(v)) edges(v) match {
        case `endVertex` => Vertex.end
        case u: ValueVertex[A1] => {
          if (contains(u)) u
          else successor(u)
        }
      }
      else throw new IllegalArgumentException(s"Sequence does not contain $v")
    }
  }

  def addRight(position: Vertex, a: A): RGA[A] = addRight(position, new ValueVertex[A](a, Vertex.genTimestamp))

  def addRight(position: Vertex, v: ValueVertex[A]): RGA[A] = insert(position, v)

  def append(v: ValueVertex[A]): RGA[A] = {
    val position = if (vertexIterator.nonEmpty) vertexIterator.toList.last else Vertex.start
    insert(position, v)
  }

  def prepend(v: ValueVertex[A]): RGA[A] = {
    insert(Vertex.start, v)
  }

  override def remove(v: ValueVertex[A]): RGA[A] = RGA((vertices.remove(v), edges))

  override def vertices: TwoPSet[ValueVertex[A]] = payload._1

  def value: List[A] = iterator.toList

  def iterator: Iterator[A] = vertexIterator.map(v => v.value)

  def vertexIterator: Iterator[ValueVertex[A]] = new AbstractIterator[ValueVertex[A]] {
    var lastVertex: Vertex = `startVertex`

    override def hasNext: Boolean = successor(lastVertex) match {
      case `endVertex` => false
      case _ => true
    }

    override def next(): ValueVertex[A] = {
      successor(lastVertex) match {
        case v: ValueVertex[A] => lastVertex = v; v
      }
    }
  }

  def fromPayload(payload: payloadType): RGA[A] = RGA(payload)

  def fromValue(value: valueType): RGA[A] = {
    val emptyPayload: payloadType = (TwoPSet[ValueVertex[A]](), HashMap[Vertex, Vertex](Vertex.start -> Vertex.end))
    val newRGA: RGA[A] = fromPayload(emptyPayload)

    value.reverse.foldLeft(newRGA) {
      case (r: RGA[A], a) => r.addRight(Vertex.start, a)
    }
  }

  /**
    * This method allows insertions of any type into the RGA. This is used to move the start and end nodes
    *
    * @param position the vertex specifying the position
    * @param v        the vertex to be inserted right to position
    * @tparam A type of the inserted vertex
    * @return A new RAG containing the inserted element
    */
  private def insert(position: Vertex, v: ValueVertex[A]): RGA[A] = position match {
    case `endVertex` =>
      throw new IllegalArgumentException("Cannot insert after end node!")
    case _ => if (edges.contains(position)) {
      val (l, r) = (position, edges(position))
      // Check if the vertex right to us has been inserted after us.  If yes, insert v after the new vertex.
      if (r.timestamp > v.timestamp) insert(r, v)
      else {
        val newVertices = vertices.add(v)
        val newEdges = edges + (l -> v) + (v -> r)
        new RGA((newVertices, newEdges))
      }
    }
    else {
      throw new IllegalArgumentException(s"Insertion failed! Sequence does not contain specified position vertex $position!")
    }
  }
}


object RGA {
  def apply[A](values: A*): RGA[A] = {
    val r = RGA[A]()
    r.fromValue(values.toList)
  }

  def apply[A](): RGA[A] = empty

  def empty[A]: RGA[A] = new RGA[A]((TwoPSet[ValueVertex[A]](), HashMap(Vertex.start -> Vertex.end)))

  implicit def RGA2CRDTInstance[A] = new StateCRDT[List[A], RGA[A]] {
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
        case (merged: RGA[A], v: Vertex) => left.logger.debug(s"inserting $v at position ${oldPositions(v)}"); merged.insert(oldPositions(v), v)
      }.edges

      // merge vertices
      val mergedVertices = TwoPSet.TwoPSetCRDTInstance.merge(left.vertices, r.vertices)

      RGA((mergedVertices, mergedEdges))
    }
  }
}

