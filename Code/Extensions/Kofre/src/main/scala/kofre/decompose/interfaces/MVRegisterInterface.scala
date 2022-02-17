package kofre.decompose.interfaces

import kofre.decompose.*
import kofre.decompose.CRDTInterface.{DeltaMutator, DeltaQuery}
import kofre.decompose.DotStore.DotFun

object MVRegisterInterface {
  type State[A, C] = Causal[DotFun[A], C]

  trait MVRegisterCompanion {
    type State[A, C] = MVRegisterInterface.State[A, C]
    type Embedded[A] = DotFun[A]
  }

  private class DeltaStateFactory[A: UIJDLattice, C: CContext] {
    val bottom: State[A, C] = UIJDLattice[State[A, C]].bottom

    def make(
        df: DotFun[A] = bottom.dotStore,
        cc: C = bottom.cc
    ): State[A, C] = Causal(df, cc)
  }

  private def deltaState[A: UIJDLattice, C: CContext]: DeltaStateFactory[A, C] = new DeltaStateFactory[A, C]

  def read[A: UIJDLattice, C: CContext]: DeltaQuery[State[A, C], Set[A]] = {
    case Causal(df, _) => df.values.toSet
  }

  def write[A: UIJDLattice, C: CContext](v: A): DeltaMutator[State[A, C]] = {
    case (replicaID, Causal(df, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      deltaState.make(
        df = Map(nextDot -> v),
        cc = CContext[C].fromSet(df.keySet + nextDot)
      )
  }

  def clear[A: UIJDLattice, C: CContext](): DeltaMutator[State[A, C]] = {
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
abstract class MVRegisterInterface[A: UIJDLattice, C: CContext, Wrapper]
    extends CRDTInterface[MVRegisterInterface.State[A, C], Wrapper] {
  def read: Set[A] = query(MVRegisterInterface.read)

  def write(v: A): Wrapper = mutate(MVRegisterInterface.write(v))

  def clear(): Wrapper = mutate(MVRegisterInterface.clear())
}
