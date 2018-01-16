package universe

import rescala.Engines
import rescala.core.{Scheduler, Struct}

import scala.collection.parallel.ForkJoinTaskSupport

object Globals {
  val engineName = System.getProperty("engineName", "parrp")

  implicit val engine: Scheduler[Struct] = engineName match {
    case "fullmv" => new rescala.fullmv.FullMVEngine(scala.concurrent.duration.Duration.Zero, "fullmv-universe").asInstanceOf[Scheduler[Struct]]
    case "stm" => rescala.stm.STMEngine.stm.asInstanceOf[Scheduler[Struct]]
    case _ =>  Engines.byName[Struct](engineName)
  }

  var taskSupport: ForkJoinTaskSupport = _
  def setParallelism(n: Int): Unit = {
    if (taskSupport != null) taskSupport.environment.shutdown()
    taskSupport = {
      new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(n))
    }
  }

}
