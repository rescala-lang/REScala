package rescala

import rescala.core.Scheduler
import rescala.operator.Interface

trait PlatformInterfaces {
  def byName(name: String): Interface =
    name match {
      case other => throw new IllegalArgumentException(s"unknown engine $other")
    }

  def defaultPlatformScheduler: rescala.interfaces.unmanaged.scheduler.type = rescala.interfaces.unmanaged.scheduler
}
