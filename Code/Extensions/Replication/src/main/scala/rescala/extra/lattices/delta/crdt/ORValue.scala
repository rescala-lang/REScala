package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.{CContext, Causal, UIJDLattice}

object ORValueCRDT {
  type State[A, C] = Causal[DotFun[A], C]

  def read[A: UIJDLattice, C: CContext]: DeltaQuery[State[A, C], Option[A]] = {
    case Causal(df, _) => df.values.headOption
  }

  def write[A: UIJDLattice, C: CContext](v: A): DeltaMutator[State[A, C]] = {
    case (replicaID, Causal(df, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      Causal(
        Map(nextDot -> v),
        CContext[C].fromSet(DotFun[A].dots(df))
      )
  }

  def clear[A: UIJDLattice, C: CContext](): DeltaMutator[State[A, C]] = {
    case (_, Causal(df, _)) =>
      Causal(
        DotFun[A].empty,
        CContext[C].fromSet(DotFun[A].dots(df))
      )
  }
}

object ORValue {
  type State[A, C] = ORValueCRDT.State[A, C]
  type Embedded[A] = DotFun[A]
}
