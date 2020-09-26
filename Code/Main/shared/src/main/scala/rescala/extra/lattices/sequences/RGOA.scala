package rescala.extra.lattices.sequences

import rescala.extra.lattices.Lattice

import scala.collection.immutable.HashMap

object RGOA {

  type RGOA[A] = LatticeSequence[A, Set[Vertex]]

  implicit def lattice[A]: Lattice[RGOA[A]] = LatticeSequence.lattice[A, Set[Vertex]]

  def empty[A]: RGOA[A] = LatticeSequence(Set[Vertex](), HashMap[Vertex, Vertex](Vertex.start -> Vertex.end), Map())

  /** Allows the creation of new CRDTs by passing an initial value.
    *
    * @param value the value
    * @return new CRDT instance representing the value
    */
  def apply[A](value: Seq[A]): RGOA[A] = {
    value.reverse.foldLeft(empty[A]) {
      case (r: RGOA[A], a) => r.addRight(Vertex.start, a)
    }
  }
}
