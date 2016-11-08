package universe

import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn

import scala.collection.parallel.ForkJoinTaskSupport

object Globals {
  val engineName = System.getProperty("engineName", "parrp")

  implicit val engine: Engine[Struct, Turn[Struct]] = rescala.engines.Engines.byName[Struct](engineName)

  var taskSupport: ForkJoinTaskSupport = _
  def setParallelism(n: Int) = {
    if (taskSupport != null) taskSupport.environment.shutdown()
    taskSupport = {
      new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(n))
    }
  }

}
