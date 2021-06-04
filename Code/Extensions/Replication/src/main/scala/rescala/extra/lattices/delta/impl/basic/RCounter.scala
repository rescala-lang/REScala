package rescala.extra.lattices.delta.impl.basic

import rescala.extra.lattices.delta.CContext
import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.crdt.RCounterCRDT

class RCounter[C: CContext](crdt: DeltaCRDT[RCounterCRDT.State[C]]) {
  def value: Int = crdt.query(RCounterCRDT.value)

  def fresh(): RCounter[C] = new RCounter(crdt.mutate(RCounterCRDT.fresh))

  def increment(): RCounter[C] = new RCounter(crdt.mutate(RCounterCRDT.increment))

  def decrement(): RCounter[C] = new RCounter(crdt.mutate(RCounterCRDT.decrement))

  def reset(): RCounter[C] = new RCounter(crdt.mutate(RCounterCRDT.reset))

  def processReceivedDeltas(): RCounter[C] = new RCounter(crdt.processReceivedDeltas())
}

object RCounter {
  type State[C] = RCounterCRDT.State[C]
  type Embedded = DotFun[(Int, Int)]

  def apply[C: CContext](antiEntropy: AntiEntropy[State[C]]): RCounter[C] =
    new RCounter(DeltaCRDT.empty(antiEntropy))
}
