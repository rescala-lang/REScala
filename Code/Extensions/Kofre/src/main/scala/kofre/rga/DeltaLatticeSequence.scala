package kofre.rga

import kofre.base.Defs.Id
import kofre.base.Lattice
import kofre.causality.CausalContext
import kofre.decompose.interfaces.EnableWinsFlag
import kofre.predef.AddWinsSet
import kofre.predef.AddWinsSet
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper, PermIdMutate, WithNamedContext}
import kofre.contextual.WithContext

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

case class DeltaSequence[A](vertices: AddWinsSet[Vertex], edges: DeltaSequenceOrder, values: Map[Vertex, A])

object DeltaSequence {

  def empty[A]: DeltaSequence[A] =
    val addStart = WithContext(AddWinsSet.empty[kofre.rga.Vertex], CausalContext.empty).named(Vertex.start.id).add(
      Vertex.start
    ).inner
    DeltaSequence(
      addStart.store,
      DeltaSequenceOrder(Map()),
      Map.empty,
    )

  implicit class DeltaSequenceOps[C, A](container: C)(using ArdtOpsContains[C, DeltaSequence[A]])
      extends OpsSyntaxHelper[C, DeltaSequence[A]](container) {

    def successor(v: Vertex)(using QueryP): Option[Vertex] = {
      current.edges.inner.get(v) match {
        case None => None
        case Some(u) =>
          if (current.vertices.contains(u)) Some(u) else successor(u)
      }
    }

    def addRightDelta(replica: Id, left: Vertex, insertee: Vertex, value: A)(using
        CausalMutation,
    ): DeltaSequence[A] = {
      val newEdges    = current.edges.addRightEdgeDelta(left, insertee)
      val newVertices = context.wrap(current.vertices).named(replica).add(insertee)
      val newValues   = Map(insertee -> value)
      newVertices.context.wrap(DeltaSequence(newVertices.store, newEdges, newValues))
    }

    def prependDelta(replica: Id, value: A)(using CausalMutation): DeltaSequence[A] =
      addRightDelta(replica, Vertex.start, Vertex.fresh(), value)

    def removeDelta(v: Vertex)(using CausalMutation): DeltaSequence[A] =
      context.wrap(current.vertices).remove(v).map(vert => copy(vertices = vert))


    def filterDelta(keep: A => Boolean)(using CausalMutation): DeltaSequence[A] = {
      val removed = values.collect { case (k, v) if !keep(v) => k }
      removed.map(this.removeDelta).foldLeft(DeltaSequence.empty[A]) { case (l, r) => Lattice.merge(l, r) }
    }

    def toList(using QueryP) = iterator.toList

    def iterator(using QueryP): Iterator[A] = vertexIterator.map(v => values(v))

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

  implicit def deltaSequenceLattice[A]: Lattice[DeltaSequence[A]] =
    new Lattice[DeltaSequence[A]] {

      private val noMapConflictsLattice: Lattice[A] = new Lattice[A] {
        override def merge(left: A, right: A): A =
          if (left == right) left
          else throw new IllegalStateException(s"assumed there would be no conflict, but have $left and $right")
      }

      override def merge(left: DeltaSequence[A], right: DeltaSequence[A]): DeltaSequence[A] = {
        val newVertices = right.vertices.elements.filter(!left.edges.inner.contains(_))

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

        DeltaSequence(
          vertices = vertices,
          edges = newEdges,
          values = values.view.filterKeys(vertices.contains).toMap: @scala.annotation.nowarn()
        )
      }
    }
}
