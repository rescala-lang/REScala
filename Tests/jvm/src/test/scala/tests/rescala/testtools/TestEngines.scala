package tests.rescala.testtools

import rescala.core.Struct
import rescala.interface.RescalaInterface
import rescala.stm.STMScheduler
import rescala.{Interfaces, Schedulers}

object TestEngines {
  val all: List[RescalaInterface[_ <: Struct]] = List(RescalaInterface.interfaceFor(Schedulers.unmanaged),
                                                      Interfaces.parrp,
                                                      RescalaInterface.interfaceFor(STMScheduler.stm))
}
