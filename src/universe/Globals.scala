package universe

import rescala.graph.Spores
import rescala.turns.{Engine, Turn}

import scala.collection.parallel.ForkJoinTaskSupport

object Globals {
  implicit val engine: Engine[Spores, Turn[Spores]] = rescala.synchronization.Engines.byName[Spores](System.getProperty("engineName", "parrp"))

  val engineName =
    if (engine == rescala.synchronization.Engines.parrp) "parrp"
    else if (engine == rescala.synchronization.Engines.stm) "stm"
    else if (engine == rescala.turns.Engines.synchron) "synchron" else "unknown"

  var taskSupport: ForkJoinTaskSupport = _
  def setParallelism(n: Int) = {
    if (taskSupport != null) taskSupport.environment.shutdown()
    taskSupport = {
      new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(n))
    }
  }

}
