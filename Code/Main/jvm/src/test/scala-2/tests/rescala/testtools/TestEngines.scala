package tests.rescala.testtools

import rescala.Schedulers._
import rescala.interface.RescalaInterface

object TestEngines {
  val all: List[RescalaInterface] = List(synchron, parrp, simple)
}
