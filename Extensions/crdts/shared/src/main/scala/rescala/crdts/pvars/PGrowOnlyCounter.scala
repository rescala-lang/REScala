package rescala.crdts.pvars

import rescala.default.implicitScheduler
import rescala.default.Evt
import rescala.crdts.pvars.Publishable.PVarFactory
import rescala.crdts.statecrdts.counters.GCounter

/**
  * DistributedGCounters are increase-only counter variables.
  *
  * @param initial The initial value of this variable.
  */
case class PGrowOnlyCounter(initial: GCounter = GCounter(0),
                            internalChanges: Evt[GCounter] = Evt[GCounter],
                            externalChanges: Evt[GCounter] = Evt[GCounter])
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

  implicit object PGrowOnlyCounterFactory extends PVarFactory[PGrowOnlyCounter] {
    override def apply(): PGrowOnlyCounter = PGrowOnlyCounter()
  }

}
