package tests.rescala.testtools

import rescala.Schedulers
import rescala.interface.RescalaInterface
import rescala.simpleprop.SimpleScheduler

object TestEngines {
  val all  = Seq(RescalaInterface.interfaceFor(Schedulers.synchron), RescalaInterface.interfaceFor(SimpleScheduler))
}
