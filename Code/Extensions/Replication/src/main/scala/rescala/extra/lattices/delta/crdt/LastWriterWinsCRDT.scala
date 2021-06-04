package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta._

object LastWriterWinsCRDT {
  type State[A, C] = MVRegisterCRDT.State[TimedVal[A], C]

  def read[A, C: CContext]: DeltaQuery[State[A, C], Option[A]] =
    MVRegisterCRDT.read[TimedVal[A], C].andThen(s => s.reduceOption(UIJDLattice[TimedVal[A]].merge).map(_.value))

  def write[A, C: CContext](v: A): DeltaMutator[State[A, C]] =
    (replicaID, state) => MVRegisterCRDT.write(TimedVal(v, replicaID)).apply(replicaID, state)

  def map[A, C: CContext](f: A => A): DeltaMutator[State[A, C]] = (replicaID, state) =>
    read[A, C].apply(state).map(f) match {
      case None    => UIJDLattice[State[A, C]].bottom
      case Some(v) => write(v).apply(replicaID, state)
    }

  def clear[A, C: CContext](): DeltaMutator[State[A, C]] = MVRegisterCRDT.clear
}
