package universe

import java.util.concurrent.ForkJoinPool

import rescala.Schedulers
import rescala.interface.RescalaInterface

import scala.collection.parallel.ForkJoinTaskSupport

object Globals {
  val engineName: String = System.getProperty("engineName", "parrp")

  implicit val engine: RescalaInterface = Schedulers.byName(engineName)

  var taskSupport: ForkJoinTaskSupport = _
  def setParallelism(n: Int): Unit = {
    if (taskSupport != null) taskSupport.environment.shutdown()
    taskSupport = {
      new ForkJoinTaskSupport(new ForkJoinPool(n))
    }
  }

}
