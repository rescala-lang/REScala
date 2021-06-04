package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta._

object ORValueCRDT {
  type State[A, C] = Causal[DotFun[A], C]
  type Embedded[A] = DotFun[A]

  private def deltaState[A: UIJDLattice, C: CContext](
      df: Option[DotFun[A]] = None,
      cc: C
  ): State[A, C] = {
    val bottom = UIJDLattice[State[A, C]].bottom

    Causal(
      df.getOrElse(bottom.dotStore),
      cc
    )
  }

  def read[A: UIJDLattice, C: CContext]: DeltaQuery[State[A, C], Option[A]] = {
    case Causal(df, _) => df.values.headOption
  }

  def write[A: UIJDLattice, C: CContext](v: A): DeltaMutator[State[A, C]] = {
    case (replicaID, Causal(df, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      deltaState(
        df = Some(Map(nextDot -> v)),
        cc = CContext[C].fromSet(DotFun[A].dots(df))
      )
  }

  def clear[A: UIJDLattice, C: CContext](): DeltaMutator[State[A, C]] = {
    case (_, Causal(df, _)) =>
      deltaState(
        cc = CContext[C].fromSet(DotFun[A].dots(df))
      )
  }
}
