package rescala.extra.lattices.delta.interfaces

import rescala.extra.lattices.delta.CRDTInterface.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta._

object MVRegisterInterface {
  type State[A, C] = Causal[DotFun[A], C]

  trait MVRegisterCompanion {
    type State[A, C] = MVRegisterInterface.State[A, C]
    type Embedded[A] = DotFun[A]
  }

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

  def clear[A: UIJDLattice, C: CContext](): DeltaMutator[State[A, C]] = {
    case (_, Causal(df, _)) =>
      deltaState(
        cc = Some(CContext[C].fromSet(df.keySet))
      )
  }
}

abstract class MVRegisterInterface[A: UIJDLattice, C: CContext, Wrapper]
    extends CRDTInterface[MVRegisterInterface.State[A, C], Wrapper] {
  def read: Set[A] = query(MVRegisterInterface.read)

  def write(v: A): Wrapper = mutate(MVRegisterInterface.write(v))

  def clear(): Wrapper = mutate(MVRegisterInterface.clear())
}
