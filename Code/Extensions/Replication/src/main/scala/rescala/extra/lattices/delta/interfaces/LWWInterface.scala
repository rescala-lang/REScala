package rescala.extra.lattices.delta.interfaces

import rescala.extra.lattices.delta.CRDTInterface.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta._

object LWWInterface {
  type State[A, C] = MVRegisterInterface.State[TimedVal[A], C]

  trait LWWCompanion {
    type State[A, C] = LWWInterface.State[A, C]
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

abstract class LWWInterface[A, C: CContext, Wrapper] extends CRDTInterface[LWWInterface.State[A, C], Wrapper] {
  def read: Option[A] = query(LWWInterface.read)

  def write(v: A): Wrapper = mutate(LWWInterface.write(v))

  def map(f: A => A): Wrapper = mutate(LWWInterface.map(f))

  def clear(): Wrapper = mutate(LWWInterface.clear())
}
