package rescala

import rescala.core.Scheduler
import rescala.levelbased.{LevelBasedSchedulers, SimpleStruct}

object Schedulers extends LevelBasedSchedulers {
  val all: List[Scheduler[SimpleStruct]] = List(synchron, unmanaged)
}
