package tests.rescala.testtools

import rescala.Engines
import rescala.interface.RescalaInterface

object TestEngines {
  val all  = Seq(RescalaInterface.interfaceFor(Engines.synchron))
}
