package rescala.scheduler

import rescala.core.Scheduler
import rescala.operator.Interface

trait PlatformSchedulers {
  def byName(name: String): Interface =
    name match {
      case other => throw new IllegalArgumentException(s"unknown engine $other")
    }

  type defaultPlatformState[V] = Schedulers.unmanaged.bundle.State[V]
  def defaultPlatformScheduler: Scheduler[defaultPlatformState] = Schedulers.unmanaged.scheduler
}
