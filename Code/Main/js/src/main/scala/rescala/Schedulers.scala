package rescala

import rescala.core.Scheduler
import rescala.scheduler.levelbased.{LevelBasedSchedulers, LevelStructImpl}

object Schedulers extends LevelBasedSchedulers {
  val all: List[Scheduler[LevelStructImpl]] = List(synchron, unmanaged)
}
