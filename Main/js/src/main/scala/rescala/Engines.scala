package rescala

import rescala.core.Engine
import rescala.levelbased.LevelBasedPropagationEngines

object Engines extends LevelBasedPropagationEngines {
  implicit val default: SimpleEngine = synchron

  val all: List[SimpleEngine] = List(synchron, unmanaged)

  val locksweep: Engine[_] = null
  val parallellocksweep: Engine[_] = null

}
