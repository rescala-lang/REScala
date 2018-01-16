package rescala

import rescala.core.Scheduler
import rescala.levelbased.LevelBasedPropagationEngines

object Engines extends LevelBasedPropagationEngines {
  implicit val default: SimpleEngine = synchron

  val all: List[SimpleEngine] = List(synchron, unmanaged)

  val locksweep: Scheduler[_] = null
  val parallellocksweep: Scheduler[_] = null

}
