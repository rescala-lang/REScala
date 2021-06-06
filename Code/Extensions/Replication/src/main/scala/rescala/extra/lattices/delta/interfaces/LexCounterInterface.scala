package rescala.extra.lattices.delta.interfaces

import rescala.extra.lattices.delta.CRDTInterface.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta._

object LexCounterInterface {
  type State = Map[String, LexPair[Int, Int]]

  trait LexCounterCompanion {
    type State = LexCounterInterface.State
  }

  def value: DeltaQuery[State, Int] = state => state.values.map(_.snd).sum

  def inc(): DeltaMutator[State] = (replicaID, state) =>
    state.updatedWith(replicaID) {
      case None                => Some(LexPair(0, 1))
      case Some(LexPair(l, r)) => Some(LexPair(l, r + 1))
    }

  def dec(): DeltaMutator[State] = (replicaID, state) =>
    state.updatedWith(replicaID) {
      case None                => Some(LexPair(1, -1))
      case Some(LexPair(l, r)) => Some(LexPair(l + 1, r - 1))
    }
}

abstract class LexCounterInterface[Wrapper] extends CRDTInterface[LexCounterInterface.State, Wrapper] {
  def value: Int = query(LexCounterInterface.value)

  def inc(): Wrapper = mutate(LexCounterInterface.inc())

  def dec(): Wrapper = mutate(LexCounterInterface.dec())
}
