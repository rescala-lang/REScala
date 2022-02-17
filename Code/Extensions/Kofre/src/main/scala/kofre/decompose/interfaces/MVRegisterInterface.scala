package kofre.decompose.interfaces

import kofre.causality.{CContext, Causal, CausalContext}
import kofre.decompose.*
import kofre.decompose.CRDTInterface.{DeltaMutator, DeltaQuery}
import kofre.decompose.DotStore.DotFun

object MVRegisterInterface {
  type State[A] = Causal[DotFun[A]]
  type C = CausalContext

  trait MVRegisterCompanion {
    type State[A] = MVRegisterInterface.State[A]
    type Embedded[A] = DotFun[A]
  }

  private class DeltaStateFactory[A: UIJDLattice] {
    val bottom: State[A] = UIJDLattice[State[A]].bottom

    def make(
              df: DotFun[A] = bottom.store,
              cc: CausalContext = bottom.context
    ): State[A] = Causal(df, cc)
  }

  private def deltaState[A: UIJDLattice]: DeltaStateFactory[A] = new DeltaStateFactory[A]

  def read[A: UIJDLattice]: DeltaQuery[State[A], Set[A]] = {
    case Causal(df, _) => df.values.toSet
  }

  def write[A: UIJDLattice](v: A): DeltaMutator[State[A]] = {
    case (replicaID, Causal(df, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      deltaState.make(
        df = Map(nextDot -> v),
        cc = CContext[C].fromSet(df.keySet + nextDot)
      )
  }

  def clear[A: UIJDLattice](): DeltaMutator[State[A]] = {
    case (_, Causal(df, _)) =>
      deltaState.make(
        cc = CContext[C].fromSet(df.keySet)
      )
  }
}

/** An MVRegister (Multi-Value Register) is a Delta CRDT modeling a register.
  *
  * In the absence of concurrent writes, the MVRegister is either empty or holds one value.
  * When multiple values are written concurrently, reading the MVRegister returns a set holding all these values.
  */
abstract class MVRegisterInterface[A: UIJDLattice, Wrapper]
    extends CRDTInterface[MVRegisterInterface.State[A], Wrapper] {
  def read: Set[A] = query(MVRegisterInterface.read)

  def write(v: A): Wrapper = mutate(MVRegisterInterface.write(v))

  def clear(): Wrapper = mutate(MVRegisterInterface.clear())
}
