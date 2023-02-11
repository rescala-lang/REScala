package rescala

import rescala.interface.RescalaInterface

trait PlatformSchedulers {
  def byName(name: String): RescalaInterface =
    name match {
      case other => throw new IllegalArgumentException(s"unknown engine $other")
    }
}
