package universe

import rescala.graph.ParRPSpores
import rescala.synchronization.ParRP
import rescala.turns.Engine
import rescala.turns.Engines.NoLockEngine

object AEngine {
  implicit val engine: NoLockEngine = rescala.turns.Engines.synchron
  //implicit val engine: Engine[ParRPSpores.type, ParRP] = rescala.synchronization.Engines.parrp
}
