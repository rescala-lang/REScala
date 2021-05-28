package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta.UIJDLattice

case class FW[E](counter: Long, value: E)

object ForcedWriteCRDT {
  type State[E] = FW[E]

  implicit def ForcedWriteAsUIJDLattice[E: UIJDLattice]: UIJDLattice[State[E]] = new UIJDLattice[FW[E]] {
    override def leq(left: FW[E], right: FW[E]): Boolean = (left, right) match {
      case (FW(cLeft, vLeft), FW(cRight, vRight)) =>
        cLeft < cRight || (cLeft == cRight && UIJDLattice[E].leq(vLeft, vRight))
    }

    /** Decomposes a lattice state into ic unique irredundant join decomposition of join-irreducible states */
    override def decompose(state: FW[E]): Set[FW[E]] = state match {
      case FW(c, v) =>
        UIJDLattice[E].decompose(v).map(FW(c, _))
    }

    override def bottom: FW[E] = FW(0, UIJDLattice[E].bottom)

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: FW[E], right: FW[E]): FW[E] = (left, right) match {
      case (FW(cLeft, vLeft), FW(cRight, vRight)) =>
        if (cLeft > cRight) left
        else if (cRight > cLeft) right
        else FW(cLeft, UIJDLattice[E].merge(vLeft, vRight))
    }
  }

  def read[E]: DeltaQuery[State[E], E] = {
    case FW(_, v) => v
  }

  def mutate[E](m: DeltaMutator[E]): DeltaMutator[State[E]] = {
    case (replicaID, FW(c, v)) => FW(c, m(replicaID, v))
  }

  def write[E](writeVal: E): DeltaMutator[State[E]] = {
    case (_, FW(c, _)) => FW(c, writeVal)
  }

  def forcedWrite[E](writeVal: E): DeltaMutator[State[E]] = {
    case (_, FW(c, _)) => FW(c + 1, writeVal)
  }
}

object ForcedWrite {
  type State[E] = ForcedWriteCRDT.State[E]
}
