package rescala.crdts.pvars

import rescala.crdts.pvars.DistributedSignal.PVarFactory
import rescala.crdts.statecrdts.counters.GCounter
import rescala.default.implicitScheduler

/**
  * DistributedGCounters are increase-only counter variables.
  *
  * @param initial The initial value of this variable.
  */
case class PGrowOnlyCounter(initial: GCounter = GCounter(0))
extends DistributedSignal[Int, GCounter](initial) {
  def increase: Int = {
    localDeviceChange.fire(crdtSignal.readValueOnce.increase)
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
