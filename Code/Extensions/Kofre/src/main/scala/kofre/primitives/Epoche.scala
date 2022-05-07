package kofre.primitives

import kofre.base.Defs
import kofre.decompose.DecomposeLattice
import kofre.decompose.interfaces.EpocheInterface

case class Epoche[E](counter: Defs.Time, value: E)

object Epoche {

  given epocheAsUIJDLattice[E: DecomposeLattice]: DecomposeLattice[Epoche[E]] = new DecomposeLattice[Epoche[E]] {
    override def lteq(left: Epoche[E], right: Epoche[E]): Boolean = (left, right) match {
      case (Epoche(cLeft, vLeft), Epoche(cRight, vRight)) =>
        cLeft < cRight || (cLeft == cRight && DecomposeLattice[E].lteq(vLeft, vRight))
    }

    /** Decomposes a lattice state into ic unique irredundant join decomposition of join-irreducible states */
    override def decompose(state: Epoche[E]): Iterable[Epoche[E]] = state match {
      case Epoche(c, v) =>
        DecomposeLattice[E].decompose(v).map(Epoche(c, _))
    }

    override def empty: Epoche[E] = Epoche(0, DecomposeLattice[E].empty)

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: Epoche[E], right: Epoche[E]): Epoche[E] = (left, right) match {
      case (Epoche(cLeft, vLeft), Epoche(cRight, vRight)) =>
        if (cLeft > cRight) left
        else if (cRight > cLeft) right
        else Epoche(cLeft, DecomposeLattice[E].merge(vLeft, vRight))
    }
  }
}
