package universe

import rescala.graph.{STMSpores, ParRPSpores}
import rescala.synchronization.{STMSync, ParRP}
import rescala.turns.Engine
import rescala.turns.Engines.NoLockEngine


import scala.collection.parallel.ForkJoinTaskSupport

object Globals {
  //implicit val engine: NoLockEngine = rescala.turns.Engines.synchron
  implicit val engine: Engine[ParRPSpores.type, ParRP] = rescala.synchronization.Engines.parrp
  //implicit val engine: Engine[STMSpores.type, STMSync] = rescala.synchronization.Engines.stm

  var taskSupport: ForkJoinTaskSupport = _
  def setParallelism(n: Int) = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(n))
}
