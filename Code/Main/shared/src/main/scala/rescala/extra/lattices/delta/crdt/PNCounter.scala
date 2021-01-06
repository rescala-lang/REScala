package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.{Delta, DeltaCRDT, UIJDLatticeWithBottom}
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.UIJDLatticeWithBottom.PairAsUIJDLattice

object PNCounter {
  type State = (GCounter.State, GCounter.State)

  def apply(replicaID: String): DeltaCRDT[State] =
    DeltaCRDT.empty[State](replicaID)

  def value: DeltaQuery[State, Int] = {
    case (incCounter, decCounter) => GCounter.value(incCounter) - GCounter.value(decCounter)
  }

  def inc: DeltaMutator[State] = {
    case (replicaID, (incCounter, _)) =>
      GCounter.inc(replicaID, incCounter) match {
        case Delta(_, newIncCounter) =>
          Delta(
            replicaID,
            (newIncCounter, UIJDLatticeWithBottom[GCounter.State].bottom)
          )
      }
  }

  def dec: DeltaMutator[State] = {
    case (replicaID, (_, decCounter)) =>
      GCounter.inc(replicaID, decCounter) match {
        case Delta(_, newDecCounter) =>
          Delta(
            replicaID,
            (UIJDLatticeWithBottom[GCounter.State].bottom, newDecCounter)
          )
      }
  }
}
