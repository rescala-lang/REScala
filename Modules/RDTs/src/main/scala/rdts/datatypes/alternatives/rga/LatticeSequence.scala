package rdts.datatypes.alternatives.rga

import rdts.base.Lattice

import scala.collection.AbstractIterator

case class LatticeSequence[A, VertexSet](vertices: VertexSet, edges: Map[Vertex, Vertex], values: Map[Vertex, A])(
    implicit val vertexSet: SetLike[Vertex, VertexSet]
) {

  def contains(v: Vertex): Boolean =
    v match {
      case Vertex.start => true
      case Vertex.end   => true
      case v: Vertex    => vertexSet.contains(vertices, v)
    }

  def before(u: Vertex, v: Vertex): Boolean =
    u match {
      case Vertex.start => true
      case Vertex.end   => false
      case u: Vertex    => edges(u) == v || before(edges(u), v)
      case null         => throw new IllegalArgumentException(s"CRDTSequence does not contain Vertex $u!")
    }

  def successor(v: Vertex): Vertex = {
    edges.get(v) match {
      case None    => throw new IllegalArgumentException(s"CRDTSequence does not contain $v")
      case Some(u) => if (contains(u)) u else successor(u)
    }
  }

  def addRight(position: Vertex, a: A): LatticeSequence[A, VertexSet] = {
    addRight(position, Vertex.fresh(), a)
  }

  /** This method allows insertions of any type into the RGA. This is used to move the start and end nodes
    *
    * @param left     the vertex specifying the position
    * @param insertee the vertex to be inserted right to position
    * @return A new RAG containing the inserted element
    */
  def addRight(left: Vertex, insertee: Vertex, value: A): LatticeSequence[A, VertexSet] = {
    if (left == Vertex.end) throw new IllegalArgumentException("Cannot insert after end node!")

    val right = successor(left)
    // sort order during merge based on most recent on towards start
    if (right.timestamp > insertee.timestamp) addRight(right, insertee, value)
    else {
      val newVertices = vertexSet.add(vertices, insertee)
      val newEdges    = edges + (left -> insertee) + (insertee -> right)
      val newValues   = values.updated(insertee, value)
      copy(newVertices, newEdges, newValues)(vertexSet)
    }
  }

  def append(value: A): LatticeSequence[A, VertexSet] = {
    val position = if (vertexIterator.nonEmpty) vertexIterator.toList.last else Vertex.start
    addRight(position, value)
  }

  def prepend(value: A): LatticeSequence[A, VertexSet] = addRight(Vertex.start, value)

  def toList: List[A] = iterator.toList

  def iterator: Iterator[A] = vertexIterator.map(v => values(v))

  def vertexIterator: Iterator[Vertex] =
    new AbstractIterator[Vertex] {
      var lastVertex: Vertex = Vertex.start

      override def hasNext: Boolean =
        successor(lastVertex) match {
          case Vertex.end => false
          case _          => true
        }

      override def next(): Vertex = {
        successor(lastVertex) match {
          case v: Vertex => lastVertex = v; v
          case null => throw new NoSuchElementException(
              "Requesting iterator value after Vertex.end!"
            )
        }
      }
    }
}

object LatticeSequence {
  implicit def lattice[A, VS: Lattice](implicit sl: SetLike[Vertex, VS]): Lattice[LatticeSequence[A, VS]] =
    new Lattice[LatticeSequence[A, VS]] {
      override def merge(left: LatticeSequence[A, VS], right: LatticeSequence[A, VS]): LatticeSequence[A, VS] = {
        val newVertices = right.vertexIterator.toList.filter(!left.edges.contains(_))

        // build map of old insertion positions of the new vertices
        val oldPositions = right.edges.foldLeft(Map(): Map[Vertex, Vertex]) {
          case (m, (u, v)) => if (newVertices.contains(v)) m + (v -> u) else m
        }

        val partialnew = newVertices.foldLeft(left) {
          case (merged, v) =>
            merged.addRight(oldPositions(v), v, right.values(v))
        }

        val vertices = Lattice.merge(left.vertices, right.vertices)

        partialnew.copy(
          vertices = vertices,
          edges = partialnew.edges,
          values = partialnew.values.filterKeys(sl.contains(vertices, _)).toMap: @scala.annotation.nowarn()
        )(partialnew.vertexSet)
      }
    }
}
