package kofre.decompose.interfaces

import kofre.decompose.CRDTInterface.{DeltaMutator, DeltaQuery}
import kofre.decompose.UIJDLattice

object ForcedWriteInterface {
  case class State[E](counter: Long, value: E)

  implicit def ForcedWriteAsUIJDLattice[E: UIJDLattice]: UIJDLattice[State[E]] = new UIJDLattice[State[E]] {
    override def leq(left: State[E], right: State[E]): Boolean = (left, right) match {
      case (State(cLeft, vLeft), State(cRight, vRight)) =>
        cLeft < cRight || (cLeft == cRight && UIJDLattice[E].leq(vLeft, vRight))
    }

    /** Decomposes a lattice state into ic unique irredundant join decomposition of join-irreducible states */
    override def decompose(state: State[E]): Iterable[ForcedWriteInterface.State[E]] = state match {
      case State(c, v) =>
        UIJDLattice[E].decompose(v).map(State(c, _))
    }

    override def bottom: State[E] = State(0, UIJDLattice[E].bottom)

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: State[E], right: State[E]): State[E] = (left, right) match {
      case (State(cLeft, vLeft), State(cRight, vRight)) =>
        if (cLeft > cRight) left
        else if (cRight > cLeft) right
        else State(cLeft, UIJDLattice[E].merge(vLeft, vRight))
    }
  }

  def read[E]: DeltaQuery[State[E], E] = {
    case State(_, v) => v
  }

  def mutate[E](m: DeltaMutator[E]): DeltaMutator[State[E]] = {
    case (replicaID, State(c, v)) => State(c, m(replicaID, v))
  }

  def write[E](writeVal: E): DeltaMutator[State[E]] = {
    case (_, State(c, _)) => State(c, writeVal)
  }

  def forcedWrite[E](writeVal: E): DeltaMutator[State[E]] = {
    case (_, State(c, _)) => State(c + 1, writeVal)
  }
}
