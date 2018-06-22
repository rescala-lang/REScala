package rescala

import rescala.core.Scheduler
import rescala.levelbased.{LevelBasedPropagationEngines, SimpleStruct}

object Engines extends LevelBasedPropagationEngines {
  implicit val default: Scheduler[SimpleStruct] = synchron

  val all: List[Scheduler[SimpleStruct]] = List(synchron, unmanaged)

  val locksweep: Scheduler[_] = null
  val parallellocksweep: Scheduler[_] = null

}
