package rescala.testhelper

import rescala.Engines
import rescala.stm.STMEngine

object TestEngines {
  val all: List[Engines.TEngine] = rescala.Engines.all ::: STMEngine.stm :: Nil
}
