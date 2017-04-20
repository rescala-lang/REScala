package universe

import rescala.Engines
import rescala.engine.{Engine, Turn}
import rescala.graph.Struct

import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool

object Globals {
  val engineName = System.getProperty("engineName", "parrp")

  implicit val engine: Engine[Struct, Turn[Struct]] = Engines.byName[Struct](engineName)

  var taskSupport: ForkJoinTaskSupport = _
  def setParallelism(n: Int) = {
    if (taskSupport != null) taskSupport.environment.shutdown()
    taskSupport = {
      new ForkJoinTaskSupport(new ForkJoinPool)
    }
  }

}
