package rescala.extra.lattices.delta.interfaces

import rescala.extra.lattices.delta.CRDTInterface.{DeltaMutator, DeltaQuery}
import rescala.extra.lattices.delta._

object PNCounterInterface {
  type State = (GCounterInterface.State, GCounterInterface.State)

  trait PNCounterCompanion {
    type State = PNCounterInterface.State
  }

  private def deltaState(
      pos: GCounterInterface.State = UIJDLattice[GCounterInterface.State].bottom,
      neg: GCounterInterface.State = UIJDLattice[GCounterInterface.State].bottom
  ): State = (pos, neg)

  def value: DeltaQuery[State, Int] = {
    case (incCounter, decCounter) => GCounterInterface.value(incCounter) - GCounterInterface.value(decCounter)
  }

  def inc(): DeltaMutator[State] = {
    case (replicaID, (incCounter, _)) =>
      deltaState(pos = GCounterInterface.inc()(replicaID, incCounter))
  }

  def dec(): DeltaMutator[State] = {
    case (replicaID, (_, decCounter)) =>
      deltaState(neg = GCounterInterface.inc()(replicaID, decCounter))
  }
}

/** A PNCounter (Positive-Negative Counter) is a Delta CRDT modeling a counter.
  *
  * It is composed of two grow-only counters (see [[GCounterInterface]]) to enable both increments and decrements of the counter value.
  */
abstract class PNCounterInterface[Wrapper] extends CRDTInterface[PNCounterInterface.State, Wrapper] {
  def value: Int = query(PNCounterInterface.value)

  def inc(): Wrapper = mutate(PNCounterInterface.inc())

  def dec(): Wrapper = mutate(PNCounterInterface.dec())
}
