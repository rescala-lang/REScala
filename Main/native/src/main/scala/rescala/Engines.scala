package rescala

import rescala.levelbased.LevelBasedPropagationEngines

object Engines extends LevelBasedPropagationEngines {
  implicit val default: SimpleEngine = synchron

  val all: List[SimpleEngine] = List(synchron, unmanaged)

  val locksweep: TEngine = null
  val parallellocksweep: TEngine = null

}
