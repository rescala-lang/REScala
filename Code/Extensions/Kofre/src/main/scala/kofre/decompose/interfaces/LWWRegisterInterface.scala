package kofre.decompose.interfaces

import kofre.decompose.CRDTInterface.{DeltaMutator, DeltaQuery}
import kofre.decompose.DotStore.DotFun
import kofre.decompose._

object LWWRegisterInterface {
  type State[A, C] = MVRegisterInterface.State[TimedVal[A], C]

  trait LWWRegisterCompanion {
    type State[A, C] = LWWRegisterInterface.State[A, C]
    type Embedded[A] = DotFun[TimedVal[A]]
  }

  def read[A, C: CContext]: DeltaQuery[State[A, C], Option[A]] =
    MVRegisterInterface.read[TimedVal[A], C].andThen(s => s.reduceOption(UIJDLattice[TimedVal[A]].merge).map(_.value))

  def write[A, C: CContext](v: A): DeltaMutator[State[A, C]] =
    (replicaID, state) => MVRegisterInterface.write(TimedVal(v, replicaID)).apply(replicaID, state)

  def map[A, C: CContext](f: A => A): DeltaMutator[State[A, C]] = (replicaID, state) =>
    read[A, C].apply(state).map(f) match {
      case None    => UIJDLattice[State[A, C]].bottom
      case Some(v) => write(v).apply(replicaID, state)
    }

  def clear[A, C: CContext](): DeltaMutator[State[A, C]] = MVRegisterInterface.clear()
}

/** An LWW (Last Writer Wins) is a Delta CRDT modeling a register.
  *
  * If two concurrent write operations occur, the resulting LWW takes on the value of the write operation with the later timestamp.
  */
abstract class LWWRegisterInterface[A, C: CContext, Wrapper]
    extends CRDTInterface[LWWRegisterInterface.State[A, C], Wrapper] {
  def read: Option[A] = query(LWWRegisterInterface.read)

  def write(v: A): Wrapper = mutate(LWWRegisterInterface.write(v))

  def map(f: A => A): Wrapper = mutate(LWWRegisterInterface.map(f))

  def clear(): Wrapper = mutate(LWWRegisterInterface.clear())
}
