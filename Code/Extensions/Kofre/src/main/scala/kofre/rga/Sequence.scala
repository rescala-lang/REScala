package kofre.rga

import kofre.Lattice
import kofre.sets.TwoPSet

object Sequence {

  type RGA[A] = LatticeSequence[A, TwoPSet[Vertex]]

  implicit def lattice[A]: Lattice[RGA[A]] = LatticeSequence.lattice[A, TwoPSet[Vertex]]

  def apply[A](values: Seq[A]): RGA[A] = {
    values.reverseIterator.foldLeft(empty[A]) {
      case (r, a) => r.addRight(Vertex.start, a)
    }
  }

  def empty[A]: RGA[A] = LatticeSequence(TwoPSet[Vertex](), Map(Vertex.start -> Vertex.end), Map())

  implicit class RGAOps[A](rga: RGA[A]) {
    def remove(v: Seq[Vertex]): RGA[A] =
      rga.copy(vertices = rga.vertices.remove(v.toSet), values = rga.values -- v)
    def filter(keep: A => Boolean): RGA[A] = {
      val removed = rga.values.collect { case (k, v) if !keep(v) => k }
      remove(removed.toList)
    }
  }
}
