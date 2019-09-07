package rescala.extra.distributables

import rescala.default.implicitScheduler
import rescala.extra.distributables.DistributedSignal.PVarFactory
import rescala.extra.lattices.primitives.GCounter

/**
  * DistributedGCounters are increase-only counter variables.
  *
  * @param initial The initial value of this variable.
  */
case class PGrowOnlyCounter(initial: GCounter = GCounter(0))
extends DistributedSignal[Int, GCounter](initial, _.value) {
  def increase(): Unit = {
    crdtSignal.transform(_.increase)
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
