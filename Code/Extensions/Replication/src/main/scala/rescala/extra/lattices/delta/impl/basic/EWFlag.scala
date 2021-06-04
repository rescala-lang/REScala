package rescala.extra.lattices.delta.impl.basic

import rescala.extra.lattices.delta.CContext
import rescala.extra.lattices.delta.DotStore.DotSet
import rescala.extra.lattices.delta.crdt.EWFlagCRDT

class EWFlag[C: CContext](crdt: DeltaCRDT[EWFlagCRDT.State[C]]) {
  def read: Boolean = crdt.query(EWFlagCRDT.read)

  def enable(): EWFlag[C] = new EWFlag(crdt.mutate(EWFlagCRDT.enable()))

  def disable(): EWFlag[C] = new EWFlag(crdt.mutate(EWFlagCRDT.disable()))

  def processReceivedDeltas(): EWFlag[C] = new EWFlag(crdt.processReceivedDeltas())
}

object EWFlag {
  type State[C] = EWFlagCRDT.State[C]
  type Embedded = DotSet

  def apply[C: CContext](antiEntropy: AntiEntropy[EWFlagCRDT.State[C]]): EWFlag[C] =
    new EWFlag(DeltaCRDT.empty(antiEntropy))
}
