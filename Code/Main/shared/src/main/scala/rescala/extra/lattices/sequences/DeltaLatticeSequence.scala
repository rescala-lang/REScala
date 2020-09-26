package rescala.extra.lattices.sequences

import rescala.extra.lattices.IdUtil.Id
import rescala.extra.lattices.Lattice
import rescala.extra.lattices.sets.AddWinsSetO

import scala.collection.AbstractIterator

case class DeltaSequenceOrder(inner: Map[Vertex, Vertex]) {

  def addRightEdgeDelta(left: Vertex, insertee: Vertex): DeltaSequenceOrder = {
    inner.get(left) match {
      case None        => DeltaSequenceOrder(Map(left -> insertee))
      case Some(right) =>
        // sort order during merge based on most recent on towards start
        if (right.timestamp > insertee.timestamp) addRightEdgeDelta(right, insertee)
        else DeltaSequenceOrder(Map((left -> insertee), (insertee -> right)))
    }
  }

  def addRightEdge(left: Vertex, insertee: Vertex): DeltaSequenceOrder =
    DeltaSequenceOrder(inner ++ addRightEdgeDelta(left, insertee).inner)
}

case class DeltaSequence[A](vertices: AddWinsSetO[Vertex], edges: DeltaSequenceOrder, values: Map[Vertex, A]) {

  def successor(v: Vertex): Option[Vertex] = {
    edges.inner.get(v) match {
      case None => None
      case Some(u) =>
        if (vertices.contains(u)) Some(u) else successor(u)
    }
  }

  def addRightDelta(replica: Id, left: Vertex, insertee: Vertex, value: A): DeltaSequence[A] = {
    val newEdges    = edges.addRightEdgeDelta(left, insertee)
    val newVertices = vertices.addΔ(insertee, replica)
    val newValues   = Map(insertee -> value)
    DeltaSequence(newVertices, newEdges, newValues)
  }

  def addRight(replica: Id, left: Vertex, insertee: Vertex, value: A): DeltaSequence[A] = {
    Lattice.merge(this, addRightDelta(replica, left, insertee, value))
  }

  def prependDelta(replica: Id, value: A): DeltaSequence[A] =
    addRightDelta(replica, Vertex.start, Vertex.fresh(), value)

  def prepend(replica: Id, value: A): DeltaSequence[A] =
    Lattice.merge(this, prependDelta(replica, value))

  def removeDelta(v: Vertex): DeltaSequence[A] =
    copy(vertices = vertices.removeΔ(v))

  def filterDelta(keep: A => Boolean): DeltaSequence[A] = {
    val removed = values.collect { case (k, v) if !keep(v) => k }
    removed.map(this.removeDelta).foldLeft(DeltaSequence.empty[A]) { case (l, r) => Lattice.merge(l, r) }
  }

  def toList: List[A] = iterator.toList

  def iterator: Iterator[A] = vertexIterator.map(v => values(v))

  def vertexIterator: Iterator[Vertex] =
    new AbstractIterator[Vertex] {
      var lastVertex: Vertex = Vertex.start

      var currentSuccessor: Option[Vertex] = successor(lastVertex)

      override def hasNext: Boolean = currentSuccessor.isDefined

      override def next(): Vertex = {
        currentSuccessor match {
          case Some(v) =>
            lastVertex = v
            currentSuccessor = successor(v)
            v
          case _ => throw new NoSuchElementException(
              "Requesting iterator value after Vertex.end!"
            )
        }
      }
    }

}

object DeltaSequence {

  def apply[A](replica: Id, values: Seq[A]): DeltaSequence[A] = {
    values.reverseIterator.foldLeft(empty[A]) {
      case (r, a) => r.prepend(replica, a)
    }
  }

  def empty[A]: DeltaSequence[A] =
    DeltaSequence(AddWinsSetO.empty.add(Vertex.start, Vertex.start.id), DeltaSequenceOrder(Map()), Map.empty)

  implicit def deltaSequenceLattice[A]: Lattice[DeltaSequence[A]] =
    new Lattice[DeltaSequence[A]] {

      private val noMapConflictsLattice: Lattice[A] = new Lattice[A] {
        override def merge(left: A, right: A): A =
          if (left == right) left
          else throw new IllegalStateException(s"assumed there would be no conflict, but have $left and $right")
      }

      override def merge(left: DeltaSequence[A], right: DeltaSequence[A]): DeltaSequence[A] = {
        val newVertices = right.vertices.toSet.filter(!left.edges.inner.contains(_))

        // build map of old insertion positions of the new vertices
        val oldPositions = right.edges.inner.foldLeft(Map.empty[Vertex, Vertex]) {
          case (m, (u, v)) => if (newVertices.contains(v)) m + (v -> u) else m
        }

        val newEdges = newVertices.foldLeft(left.edges) {
          case (merged, v) =>
            merged.addRightEdge(oldPositions(v), v)
        }
        val vertices = Lattice.merge(left.vertices, right.vertices)
        val values   = Lattice.merge(left.values, right.values)(Lattice.mapLattice(noMapConflictsLattice))

        DeltaSequence(vertices = vertices, edges = newEdges, values = values.view.filterKeys(vertices.contains).toMap)
      }
    }
}
