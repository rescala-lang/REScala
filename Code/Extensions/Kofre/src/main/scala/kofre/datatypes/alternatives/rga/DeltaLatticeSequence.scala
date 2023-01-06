package kofre.datatypes.alternatives.rga

import kofre.base.Id
import kofre.base.{DecomposeLattice, Lattice}
import kofre.time.Dots
import kofre.datatypes.{AddWinsSet, EnableWinsFlag}
import kofre.datatypes.AddWinsSet
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper, PermIdMutate, DottedName}
import kofre.dotted.{DottedDecompose, DottedLattice, Dotted, HasDots}

import scala.collection.{AbstractIterator, immutable}

case class DeltaSequence[A](
    vertices: AddWinsSet[Vertex],
    edges: DeltaSequence.DeltaSequenceOrder,
    values: Map[Vertex, A]
)

object DeltaSequence {

  def empty[A]: DeltaSequence[A] =
    val addStart = Dotted(AddWinsSet.empty[Vertex], Dots.empty).named(Vertex.start.id).add(
      Vertex.start
    ).anon
    DeltaSequence(
      addStart.store,
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

  implicit class DeltaSequenceOps[C, A](container: C)(using ArdtOpsContains[C, DeltaSequence[A]])
      extends OpsSyntaxHelper[C, DeltaSequence[A]](container) {

    def successor(v: Vertex)(using QueryP): Option[Vertex] = {
      current.edges.inner.get(v) match {
        case None => None
        case Some(u) =>
          if (current.vertices.contains(u)) Some(u) else successor(u)
      }
    }

    def addRightDelta(replica: Id, left: Vertex, insertee: Vertex, value: A)(using CausalMutationP): C = {
      val newEdges    = current.edges.addRightEdgeDelta(left, insertee)
      val newVertices = context.wrap(current.vertices).named(replica).add(insertee).anon
      val newValues   = Map(insertee -> value)
      newVertices.context.wrap(DeltaSequence(newVertices.store, newEdges, newValues)).mutator
    }

    def prependDelta(replica: Id, value: A)(using CausalMutationP): C =
      addRightDelta(replica, Vertex.start, Vertex.fresh(), value)

    def removeDelta(v: Vertex)(using CausalMutationP): C =
      context.wrap(current.vertices).remove(v).map(vert => current.copy(vertices = vert)).mutator

    def filterDelta(keep: A => Boolean)(using CausalMutationP): C = {
      val removed: immutable.Iterable[Vertex] = current.values.collect { case (k, v) if !keep(v) => k }
      removed.foldLeft(context.wrap(current: DeltaSequence[A])) {
        case (curr, toRemove) =>
          val delta = curr.removeDelta(toRemove)
          Lattice.merge(curr, delta)
      }.mutator
    }

    def toList(using QueryP): List[A] = iterator.toList

    def iterator(using QueryP): Iterator[A] = vertexIterator.map(v => current.values(v))

    def vertexIterator(using QueryP): Iterator[Vertex] =
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

  given deltaSequenceLattice[A]: DottedDecompose[DeltaSequence[A]] =
    new DottedDecompose[DeltaSequence[A]] {

      override def decompose(a: Dotted[DeltaSequence[A]]): Iterable[Dotted[DeltaSequence[A]]] = Iterable(a)

      private val noMapConflictsLattice: Lattice[A] = (left: A, right: A) =>
        if (left == right) left
        else throw new IllegalStateException(s"assumed there would be no conflict, but have $left and $right")

      override def mergePartial(
          left: Dotted[DeltaSequence[A]],
          right: Dotted[DeltaSequence[A]]
      ): DeltaSequence[A] = {
        val newVertices = right.store.vertices.elements.filter(!left.store.edges.inner.contains(_))

        // build map of old insertion positions of the new vertices
        val oldPositions = right.store.edges.inner.foldLeft(Map.empty[Vertex, Vertex]) {
          case (m, (u, v)) => if (newVertices.contains(v)) { m + (v -> u) }
            else m
        }

        val newEdges = newVertices.foldLeft(left.store.edges) {
          case (merged, v) =>
            if (v == Vertex.start) merged
            else merged.addRightEdge(oldPositions(v), v)
        }
        val vertices = left.map(_.vertices) mergePartial right.map(_.vertices)
        val values   = Lattice.merge(left.store.values, right.store.values)(Lattice.mapLattice(noMapConflictsLattice))

        DeltaSequence(
          vertices = vertices,
          edges = newEdges,
          values = values.view.filterKeys(vertices.contains).toMap
        )
      }
    }
}
