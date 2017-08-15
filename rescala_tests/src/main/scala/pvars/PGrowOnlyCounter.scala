package pvars

import rescala.Evt
import statecrdts.counters.CIncOnlyCounter

/**
  * DistributedGCounters are increase-only counter variables.
  *
  * @param initial The initial value of this variable.
  */
case class PGrowOnlyCounter(initial: CIncOnlyCounter = CIncOnlyCounter(0),
                            internalChanges: rescala.Evt[CIncOnlyCounter] = Evt[CIncOnlyCounter],
                            externalChanges: rescala.Evt[CIncOnlyCounter] = Evt[CIncOnlyCounter])
  extends Publishable[CIncOnlyCounter] {

  def increase: Int = {
    internalChanges(crdtSignal.now.increase)
    value
  }
}

object PGrowOnlyCounter {
  /**
    * Allows creation of DistributedGCounters by passing a start value.
    */
  def apply(start: Int): PGrowOnlyCounter = {
    val init: CIncOnlyCounter = CIncOnlyCounter(start)
    new PGrowOnlyCounter(init)
  }
}