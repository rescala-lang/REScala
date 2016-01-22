package universe

import rescala.graph.{Spores, STMSpores, ParRPSpores}
import rescala.synchronization.{STMSync, ParRP}
import rescala.turns.{Turn, Engine}
import rescala.turns.Engines.NoLockEngine


import scala.collection.parallel.ForkJoinTaskSupport

object Globals {
  //implicit val engine: NoLockEngine = rescala.turns.Engines.synchron
  //implicit val engine: Engine[ParRPSpores.type, ParRP] = rescala.synchronization.Engines.parrp
  implicit val engine: Engine[Spores, Turn[Spores]] = rescala.synchronization.Engines.byName[Spores](System.getProperty("engineName", "parrp"))

  val engineName =
    if (engine == rescala.synchronization.Engines.parrp) "parrp" else
    if (engine == rescala.synchronization.Engines.stm) "stm" else
    if (engine == rescala.turns.Engines.synchron) "synchron" else "unknown"

  var taskSupport: ForkJoinTaskSupport = _
  def setParallelism(n: Int) = taskSupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(n))
}
