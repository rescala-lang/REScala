package kofre.datatypes.alternatives.rga

import kofre.base.{Bottom, Lattice, Uid}
import kofre.datatypes
import kofre.datatypes.contextual.AddWinsSet
import kofre.dotted.{Dotted, HasDots}
import kofre.syntax.OpsSyntaxHelper
import kofre.time.Dots

import scala.collection.{AbstractIterator, immutable}

case class DeltaSequence[A](
    vertices: AddWinsSet[Vertex],
    edges: DeltaSequence.DeltaSequenceOrder,
    values: Map[Vertex, A]
)

object DeltaSequence {

  given bottom[A]: Bottom[DeltaSequence[A]] with {
    override def empty: DeltaSequence[A] = DeltaSequence.empty
  }

  def empty[A]: DeltaSequence[A] =
    val addStart = Dotted(AddWinsSet.empty[Vertex]).add(using Vertex.start.id)(
      Vertex.start
    )
    DeltaSequence(
      addStart.data,
      DeltaSequenceOrder(Map()),
      Map.empty,
    )

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

  implicit class DeltaSequenceOps[C, A](container: C)
      extends OpsSyntaxHelper[C, DeltaSequence[A]](container) {

    def successor(v: Vertex)(using PermQuery): Option[Vertex] = {
      current.edges.inner.get(v) match {
        case None => None
        case Some(u) =>
          if (current.vertices.contains(u)) Some(u) else successor(u)
      }
    }

    def addRightDelta(replica: Uid, left: Vertex, insertee: Vertex, value: A)(using PermCausalMutate): C = {
      val newEdges    = current.edges.addRightEdgeDelta(left, insertee)
      val newVertices = context.wrap(current.vertices).add(using replica)(insertee)
      val newValues   = Map(insertee -> value)
      newVertices.context.wrap(DeltaSequence(newVertices.data, newEdges, newValues)).mutator
    }

    def prependDelta(replica: Uid, value: A)(using PermCausalMutate): C =
      addRightDelta(replica, Vertex.start, Vertex.fresh(), value)

    def removeDelta(v: Vertex)(using PermCausalMutate): C =
      context.wrap(current.vertices).remove(v).map(vert => current.copy(vertices = vert)).mutator

    def filterDelta(keep: A => Boolean)(using PermCausalMutate): C = {
      val removed: immutable.Iterable[Vertex] = current.values.collect { case (k, v) if !keep(v) => k }
      removed.foldLeft(context.wrap(current: DeltaSequence[A])) {
        case (curr, toRemove) =>
          val delta = curr.removeDelta(toRemove)
          Lattice.merge(curr, delta)
      }.mutator
    }

    def toList(using PermQuery): List[A] = iterator.toList

    def iterator(using PermQuery): Iterator[A] = vertexIterator.map(v => current.values(v))

    def vertexIterator(using PermQuery): Iterator[Vertex] =
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

  given hasDots[A]: HasDots[DeltaSequence[A]] with {
    extension (value: DeltaSequence[A])
      def dots: Dots = value.vertices.dots
      override def removeDots(dots: Dots): Option[DeltaSequence[A]] =
          HasDots.apply[AddWinsSet[Vertex]].removeDots(value.vertices)(dots).map { nv =>
            value.copy(vertices = nv)
          }
  }

  given deltaSequenceLattice[A]: Lattice[DeltaSequence[A]] =
    new Lattice[DeltaSequence[A]] {

      override def merge(
          left: DeltaSequence[A],
          right: DeltaSequence[A]
      ): DeltaSequence[A] = {
        val newVertices = right.vertices.elements.filter(!left.edges.inner.contains(_))

        // build map of old insertion positions of the new vertices
        val oldPositions = right.edges.inner.foldLeft(Map.empty[Vertex, Vertex]) {
          case (m, (u, v)) => if (newVertices.contains(v)) { m + (v -> u) }
            else m
        }

        val newEdges = newVertices.foldLeft(left.edges) {
          case (merged, v) =>
            if (v == Vertex.start) merged
            else merged.addRightEdge(oldPositions(v), v)
        }
        val vertices = left.vertices merge right.vertices
        val values   = Lattice.merge(left.values, right.values)(Lattice.mapLattice(Lattice.assertNoConflicts))

        DeltaSequence(
          vertices = vertices,
          edges = newEdges,
          values = values.view.filterKeys(vertices.contains).toMap
        )
      }


    }
}
