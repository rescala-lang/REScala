package universe

import rescala.graph.{STMSpores, ParRPSpores}
import rescala.synchronization.{STMSync, ParRP}
import rescala.turns.Engine
import rescala.turns.Engines.NoLockEngine

object AEngine {
  implicit val engine: NoLockEngine = rescala.turns.Engines.synchron
  //implicit val engine: Engine[ParRPSpores.type, ParRP] = rescala.synchronization.Engines.parrp
  //implicit val engine: Engine[STMSpores.type, STMSync] = rescala.synchronization.Engines.stm
}
