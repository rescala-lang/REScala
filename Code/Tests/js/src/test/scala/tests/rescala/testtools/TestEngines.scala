package tests.rescala.testtools

import rescala.Schedulers
import rescala.interface.RescalaInterface

object TestEngines {
  val all = Seq(RescalaInterface.interfaceFor(Schedulers.synchron))
}
