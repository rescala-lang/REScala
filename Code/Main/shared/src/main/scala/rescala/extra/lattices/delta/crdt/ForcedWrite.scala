package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta.UIJDLattice

object ForcedWriteCRDT {
  type State[E] = (Long, E)

  implicit def ForcedWriteAsUIJDLattice[E: UIJDLattice]: UIJDLattice[State[E]] = new UIJDLattice[(Long, E)] {
    override def leq(left: (Long, E), right: (Long, E)): Boolean = (left, right) match {
      case ((cLeft, vLeft), (cRight, vRight)) =>
        cLeft < cRight || (cLeft == cRight && UIJDLattice[E].leq(vLeft, vRight))
    }

    /** Decomposes a lattice state into ic unique irredundant join decomposition of join-irreducible states */
    override def decompose(state: (Long, E)): Set[(Long, E)] = state match {
      case (c, v) =>
        UIJDLattice[E].decompose(v).map((c, _))
    }

    override def bottom: (Long, E) = (0L, UIJDLattice[E].bottom)

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: (Long, E), right: (Long, E)): (Long, E) = (left, right) match {
      case ((cLeft, vLeft), (cRight, vRight)) =>
        if (cLeft > cRight) left
        else if (cRight > cLeft) right
        else (cLeft, UIJDLattice[E].merge(vLeft, vRight))
    }
  }

  def read[E]: DeltaQuery[State[E], E] = {
    case (_, v) => v
  }

  def mutate[E](m: DeltaMutator[E]): DeltaMutator[State[E]] = {
    case (replicaID, (c, v)) => (c, m(replicaID, v))
  }

  def write[E](writeVal: E): DeltaMutator[State[E]] = {
    case (_, (c, _)) => (c, writeVal)
  }

  def forcedWrite[E](writeVal: E): DeltaMutator[State[E]] = {
    case (_, (c, _)) => (c + 1, writeVal)
  }
}

object FW {
  def unapply[E](state: (Long, E)): Option[E] = Some(state._2)
}

object ForcedWrite {
  type State[E] = ForcedWriteCRDT.State[E]
}
