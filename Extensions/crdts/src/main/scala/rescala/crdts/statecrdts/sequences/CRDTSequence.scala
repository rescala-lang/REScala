package rescala.crdts.statecrdts.sequences

import rescala.crdts.statecrdts.sets.StateCRDTSet

import scala.collection.AbstractIterator
import scala.collection.immutable.HashMap

trait CRDTSequence[A] {
  type selfType
  type payloadType

  val start: Vertex[A] = Vertex.start[A]
  val end: Vertex[A] = Vertex.end[A]

  def vertices: StateCRDTSet[Vertex[A]]

  def edges: HashMap[Vertex[A], Vertex[A]]

  def fromPayload(payload: payloadType): selfType

  def contains(v: Vertex[A]): Boolean = v match {
    case `start` => true
    case `end` => true
    case _ => vertices.contains(v)
  }

  def containsValue(a: A): Boolean = vertices.value.flatMap(_.value).contains(a)

  def before(u: Vertex[A], v: Vertex[A]): Boolean = u match {
    case `start` => true
    case `end` => false
    case u: Vertex[A] => edges(u) == v || before(edges(u), v)
    case _ => throw new IllegalArgumentException(s"CRDTSequence does not contain Vertex $u!")
  }

  def successor[A1 >: A](v: Vertex[A]): Vertex[A] = v match {
    case `end` =>
      throw new IllegalArgumentException("There is no successor to the end node!")
    case _ =>
      if (edges.contains(v)) edges(v) match {
        case `end` => `end`
        case u: Vertex[A] =>
          if (contains(u)) u
          else successor(u)
      }
      else throw new IllegalArgumentException(s"CRDTSequence does not contain $v")
  }

  def addRight(position: Vertex[A], a: A): selfType = addRight(position, new Vertex[A](Some(a), Vertex.genTimestamp))

  /**
    * This method allows insertions of any type into the RGA. This is used to move the start and end nodes
    *
    * @param position the vertex specifying the position
    * @param v        the vertex to be inserted right to position
    * @return A new RAG containing the inserted element
    */
  def addRight(position: Vertex[A], v: Vertex[A]): selfType =
    position match {
      case `end` =>
        throw new IllegalArgumentException("Cannot insert after end node!")
      case _ => if (edges.contains(position)) {
        val (l, r) = (position, edges(position))
        // Check if the vertex right to us has been inserted after us.  If yes, insert v after the new vertex.
        if (r.timestamp > v.timestamp) addRight(r, v)
        else {
          val newVertices = vertices.add(v)
          val newEdges = edges + (l -> v) + (v -> r)
          fromPayload((newVertices, newEdges).asInstanceOf[payloadType])
        }
      }
      else {
        throw new IllegalArgumentException(s"Insertion failed! CRDTSequence does not contain specified position vertex $position!")
      }
    }

  def append(v: Vertex[A]): selfType = {
    val position = if (vertexIterator.nonEmpty) vertexIterator.toList.last else `start`
    addRight(position, v)
  }

  def prepend(v: Vertex[A]): selfType = {
    addRight(`start`, v)
  }

  def value: List[A] = iterator.toList

  def iterator: Iterator[A] = vertexIterator.flatMap(v => v.value)

  def vertexIterator: Iterator[Vertex[A]] = new AbstractIterator[Vertex[A]] {
    var lastVertex: Vertex[A] = `start`

    override def hasNext: Boolean = successor(lastVertex) match {
      case `end` => false
      case _ => true
    }

    override def next(): Vertex[A] = {
      successor(lastVertex) match {
        case v: Vertex[A] => lastVertex = v; v
      }
    }
  }
}
