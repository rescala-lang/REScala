package reactives

import reactives.operator.Interface

trait PlatformInterfaces {
  def byName(name: String): Interface =
    name match {
      case other => throw new IllegalArgumentException(s"unknown engine $other")
    }

  def defaultPlatformScheduler: reactives.interfaces.unmanaged.scheduler.type = reactives.interfaces.unmanaged.scheduler
}
