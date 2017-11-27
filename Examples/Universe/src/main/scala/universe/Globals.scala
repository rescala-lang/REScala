package universe

import rescala.Engines
import rescala.core.{Engine, Struct}

import scala.collection.parallel.ForkJoinTaskSupport

object Globals {
  val engineName = System.getProperty("engineName", "parrp")

  implicit val engine: Engine[Struct] = Engines.byName[Struct](engineName)

  var taskSupport: ForkJoinTaskSupport = _
  def setParallelism(n: Int): Unit = {
    if (taskSupport != null) taskSupport.environment.shutdown()
    taskSupport = {
      new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(n))
    }
  }

}
