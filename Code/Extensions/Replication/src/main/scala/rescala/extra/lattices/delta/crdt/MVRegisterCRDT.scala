package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta._

object MVRegisterCRDT {
  type State[A, C] = Causal[DotFun[A], C]

  private def deltaState[A: UIJDLattice, C: CContext](
      df: Option[DotFun[A]] = None,
      cc: Option[C]
  ): State[A, C] = {
    val bottom = UIJDLattice[State[A, C]].bottom

    Causal(
      df.getOrElse(bottom.dotStore),
      cc.getOrElse(bottom.cc)
    )
  }

  def read[A: UIJDLattice, C: CContext]: DeltaQuery[State[A, C], Set[A]] = {
    case Causal(df, _) => df.values.toSet
  }

  def write[A: UIJDLattice, C: CContext](v: A): DeltaMutator[State[A, C]] = {
    case (replicaID, Causal(df, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      deltaState(
        df = Some(Map(nextDot -> v)),
        cc = Some(CContext[C].fromSet(df.keySet + nextDot))
      )
  }

  def clear[A: UIJDLattice, C: CContext]: DeltaMutator[State[A, C]] = {
    case (_, Causal(df, _)) =>
      deltaState(
        cc = Some(CContext[C].fromSet(df.keySet))
      )
  }
}
