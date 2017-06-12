package rescala.testhelper

import rescala.engine.Engine
import rescala.fullmv.FullMVEngine
import rescala.graph.Struct
import rescala.stm.STMEngine

object TestEngines {
  val all: List[Engine[_ <: Struct]] = rescala.Engines.all ::: List(STMEngine.stm, FullMVEngine)
}
