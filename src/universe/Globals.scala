package universe

import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn

import scala.collection.parallel.ForkJoinTaskSupport

object Globals {
  implicit val engine: Engine[Struct, Turn[Struct]] = rescala.engines.JVMEngines.byName[Struct](System.getProperty("engineName", "parrp"))

  val engineName =
    if (engine == rescala.engines.JVMEngines.parrp) "parrp"
    else if (engine == rescala.engines.JVMEngines.stm) "stm"
    else if (engine == rescala.engines.Engines.synchron) "synchron" else "unknown"

  var taskSupport: ForkJoinTaskSupport = _
  def setParallelism(n: Int) = {
    if (taskSupport != null) taskSupport.environment.shutdown()
    taskSupport = {
      new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(n))
    }
  }

}
