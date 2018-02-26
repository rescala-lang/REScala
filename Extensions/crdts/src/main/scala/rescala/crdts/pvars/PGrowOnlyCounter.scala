package rescala.crdts.pvars

import rescala.Evt
import rescala.crdts.statecrdts.counters.GCounter

/**
  * DistributedGCounters are increase-only counter variables.
  *
  * @param initial The initial value of this variable.
  */
case class PGrowOnlyCounter(initial: GCounter = GCounter(0),
                            internalChanges: rescala.Evt[GCounter] = Evt[GCounter],
                            externalChanges: rescala.Evt[GCounter] = Evt[GCounter])
  extends Publishable[Int, GCounter] {

  def increase: Int = {
    internalChanges.fire(crdtSignal.readValueOnce.increase)
    value
  }
}

object PGrowOnlyCounter {
  /**
    * Allows creation of DistributedGCounters by passing a start value.
    */
  def apply(start: Int): PGrowOnlyCounter = {
    val init: GCounter = GCounter(start)
    new PGrowOnlyCounter(init)
  }
}
