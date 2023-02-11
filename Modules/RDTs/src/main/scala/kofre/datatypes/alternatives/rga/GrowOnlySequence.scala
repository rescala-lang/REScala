package kofre.datatypes.alternatives.rga

import kofre.base.Lattice

import scala.collection.immutable.HashMap

object GrowOnlySequence {

  type GrowOnlySequence[A] = LatticeSequence[A, Set[Vertex]]

  implicit def lattice[A]: Lattice[GrowOnlySequence[A]] = LatticeSequence.lattice[A, Set[Vertex]]

  def empty[A]: GrowOnlySequence[A] =
    LatticeSequence(Set[Vertex](), HashMap[Vertex, Vertex](Vertex.start -> Vertex.end), Map())

  /** Allows the creation of new CRDTs by passing an initial value.
    *
    * @param value the value
    * @return new CRDT instance representing the value
    */
  def apply[A](value: Seq[A]): GrowOnlySequence[A] = {
    value.reverse.foldLeft(empty[A]) {
      case (r: GrowOnlySequence[A], a) => r.addRight(Vertex.start, a)
    }
  }
}
