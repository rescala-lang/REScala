package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.{CContext, Causal, Delta, DeltaCRDT, UIJDLattice, UIJDLatticeWithBottom}

object MVRegister {
  type State[A, C] = Causal[DotFun[A], C]

  def apply[A: UIJDLattice, C: CContext](replicaID: "a"): DeltaCRDT[State[A, C]] =
    DeltaCRDT(replicaID, UIJDLatticeWithBottom[State[A, C]].bottom, List())

  def read[A, C: CContext]: DeltaQuery[State[A, C], Set[A]] = {
    case Causal(df, _) => df.values.toSet
  }

  def write[A: UIJDLattice, C: CContext](v: A): DeltaMutator[State[A, C]] = {
    case (replicaID, Causal(df, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      Delta(
        replicaID,
        Causal(
          Map(nextDot -> v),
          CContext[C].fromSet(df.keySet + nextDot)
        )
      )
  }

  def clear[A: UIJDLattice, C: CContext]: DeltaMutator[State[A, C]] = {
    case (replicaID, Causal(df, _)) =>
      Delta(
        replicaID,
        Causal(
          DotFun[A].empty,
          CContext[C].fromSet(df.keySet)
        )
      )
  }
}
