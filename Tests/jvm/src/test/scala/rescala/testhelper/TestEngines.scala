package rescala.testhelper

import rescala.Engines
import rescala.stm.STMEngine
import rescala.fullmv.FullMVEngine

object TestEngines {
  val all: List[Engines.TEngine] = rescala.Engines.all ::: List[Engines.TEngine](STMEngine.stm, FullMVEngine)
}
