package rescala.scheduler

import rescala.core.Scheduler
import rescala.operator.Interface

trait PlatformSchedulers {
  def byName(name: String): Interface =
    name match {
      case other => throw new IllegalArgumentException(s"unknown engine $other")
    }

  def defaultPlatformScheduler: Schedulers.unmanaged.scheduler.type = Schedulers.unmanaged.scheduler
}
