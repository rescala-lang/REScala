package universe

import rescala.Schedulers
import rescala.core.{Scheduler, Struct}
import rescala.interface.RescalaInterface

import scala.collection.parallel.ForkJoinTaskSupport

object Globals {
  val engineName = System.getProperty("engineName", "parrp")

  implicit val engine: RescalaInterface[Struct] = RescalaInterface.interfaceFor(engineName match {
    case "fullmv" => new rescala.fullmv.FullMVEngine(scala.concurrent.duration.Duration.Zero, "fullmv-universe").asInstanceOf[Scheduler[Struct]]
    case "stm" => rescala.stm.STMScheduler.stm.asInstanceOf[Scheduler[Struct]]
    case _ => Schedulers.byName[Struct](engineName)
  })

  var taskSupport: ForkJoinTaskSupport = _
  def setParallelism(n: Int): Unit = {
    if (taskSupport != null) taskSupport.environment.shutdown()
    taskSupport = {
      new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(n))
    }
  }

}
