package tests.rescala.testtools

import rescala.core.{Scheduler, Struct}
import rescala.stm.STMEngine

object TestEngines {
  val all: List[Scheduler[_ <: Struct]] = rescala.Engines.all ::: List(STMEngine.stm)
}
