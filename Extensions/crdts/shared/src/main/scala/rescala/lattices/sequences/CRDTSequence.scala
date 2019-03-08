package rescala.lattices.sequences

import scala.collection.AbstractIterator

trait SetLike[A, F] {
  def add(set: F, value: A): F
  def contains(set: F, value: A): Boolean
}

object SetLike {
  implicit def setInstance[A]: SetLike[A, Set[A]] = new SetLike[A, Set[A]] {
    override def add(set: Set[A], value: A): Set[A] = set + value
    override def contains(set: Set[A], value: A): Boolean = set.contains(value)
  }
}

abstract class CRDTSequence[A, VertexSet, SelfT](implicit vertexSet: SetLike[Vertex, VertexSet]) {

  val vertices: VertexSet
  val edges   : Map[Vertex, Vertex]
  val values  : Map[Vertex, A]

  def copySub(vertices: VertexSet,
              edges: Map[Vertex, Vertex],
              values: Map[Vertex, A]): SelfT

  def contains(v: Vertex): Boolean = v match {
    case Vertex.start => true
    case Vertex.end   => true
    case v: Vertex    => vertexSet.contains(vertices, v)
  }

  def before(u: Vertex, v: Vertex): Boolean = u match {
    case Vertex.start => true
    case Vertex.end   => false
    case u: Vertex    => edges(u) == v || before(edges(u), v)
    case _            => throw new IllegalArgumentException(s"CRDTSequence does not contain Vertex $u!")
  }

  def successor(v: Vertex): Vertex = {
    edges.get(v) match {
      case None    => throw new IllegalArgumentException(s"CRDTSequence does not contain $v")
      case Some(u) => if (contains(u)) u else successor(u)
    }
  }

  def addRight(position: Vertex, a: A): SelfT = addRight(position, Vertex.fresh(), a)

  /**
    * This method allows insertions of any type into the RGA. This is used to move the start and end nodes
    *
    * @param left     the vertex specifying the position
    * @param insertee the vertex to be inserted right to position
    * @return A new RAG containing the inserted element
    */
  def addRight(left: Vertex, insertee: Vertex, value: A): SelfT = {
    if (left == Vertex.end) throw new IllegalArgumentException("Cannot insert after end node!")

    val right = edges.getOrElse(left,
                                throw new IllegalArgumentException(s"Insertion failed! CRDTSequence does not contain specified position vertex $left!"))
    // Check if the vertex right to us has been inserted after us.
    // If yes, insert v after the new vertex.
    // TODO: why tough? should we not just ADD here?
    if (right.timestamp > insertee.timestamp) addRight(right, insertee, value)
    else {
      val newVertices = vertexSet.add(vertices, insertee)
      val newEdges = edges + (left -> insertee) + (insertee -> right)
      val newValues = values.updated(insertee, value)
      copySub(newVertices, newEdges, newValues)
    }
  }

  def append(value: A): SelfT = {
    val position = if (vertexIterator.nonEmpty) vertexIterator.toList.last else Vertex.start
    addRight(position, value)
  }

  def prepend(value: A): SelfT = addRight(Vertex.start, value)


  def value: List[A] = iterator.toList

  def iterator: Iterator[A] = vertexIterator.map(v => values(v))

  def vertexIterator: Iterator[Vertex] = new AbstractIterator[Vertex] {
    var lastVertex: Vertex = Vertex.start

    override def hasNext: Boolean = successor(lastVertex) match {
      case Vertex.end => false
      case _          => true
    }

    override def next(): Vertex = {
      successor(lastVertex) match {
        case v: Vertex => lastVertex = v; v
        case _         => throw new NoSuchElementException(
          "Requesting iterator value after Vertex.end!")
      }
    }
  }
}
