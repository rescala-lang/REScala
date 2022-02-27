package kofre.decompose.interfaces

import kofre.decompose.CRDTInterface.{DeltaMutator, DeltaQuery}
import kofre.decompose.UIJDLattice
import kofre.Defs

object EpocheInterface {
  case class Epoche[E](counter: Defs.Time, value: E)

  implicit def ForcedWriteAsUIJDLattice[E: UIJDLattice]: UIJDLattice[Epoche[E]] = new UIJDLattice[Epoche[E]] {
    override def leq(left: Epoche[E], right: Epoche[E]): Boolean = (left, right) match {
      case (Epoche(cLeft, vLeft), Epoche(cRight, vRight)) =>
        cLeft < cRight || (cLeft == cRight && UIJDLattice[E].leq(vLeft, vRight))
    }

    /** Decomposes a lattice state into ic unique irredundant join decomposition of join-irreducible states */
    override def decompose(state: Epoche[E]): Iterable[EpocheInterface.Epoche[E]] = state match {
      case Epoche(c, v) =>
        UIJDLattice[E].decompose(v).map(Epoche(c, _))
    }

    override def bottom: Epoche[E] = Epoche(0, UIJDLattice[E].bottom)

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: Epoche[E], right: Epoche[E]): Epoche[E] = (left, right) match {
      case (Epoche(cLeft, vLeft), Epoche(cRight, vRight)) =>
        if (cLeft > cRight) left
        else if (cRight > cLeft) right
        else Epoche(cLeft, UIJDLattice[E].merge(vLeft, vRight))
    }
  }

  def read[E]: DeltaQuery[Epoche[E], E] = {
    case Epoche(_, v) => v
  }

  def mutate[E](m: DeltaMutator[E]): DeltaMutator[Epoche[E]] = {
    case (replicaID, Epoche(c, v)) => Epoche(c, m(replicaID, v))
  }

  def write[E](writeVal: E): DeltaMutator[Epoche[E]] = {
    case (_, Epoche(c, _)) => Epoche(c, writeVal)
  }

  def forcedWrite[E](writeVal: E): DeltaMutator[Epoche[E]] = {
    case (_, Epoche(c, _)) => Epoche(c + 1, writeVal)
  }
}
