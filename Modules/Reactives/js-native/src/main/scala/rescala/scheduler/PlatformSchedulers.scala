package rescala.scheduler

import rescala.operator.Interface

trait PlatformSchedulers {
  def byName(name: String): Interface =
    name match {
      case other => throw new IllegalArgumentException(s"unknown engine $other")
    }
}
