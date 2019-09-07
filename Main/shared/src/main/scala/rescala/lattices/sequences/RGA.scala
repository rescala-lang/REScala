package rescala.lattices.sequences

import rescala.lattices.Lattice
import rescala.lattices.sets.TwoPSet


object RGA {

  type RGA[A] = LatticeSequence[A, TwoPSet[Vertex]]

  implicit def lattice[A]: Lattice[RGA[A]] = LatticeSequence.lattice[A, TwoPSet[Vertex]]

  def apply[A](values: Seq[A]): RGA[A] = {
    values.reverseIterator.foldLeft(empty[A]) {
      case (r, a) => r.addRight(Vertex.start, a)
    }
  }

  def empty[A]: RGA[A] = LatticeSequence(TwoPSet[Vertex](), Map(Vertex.start -> Vertex.end), Map())


}








