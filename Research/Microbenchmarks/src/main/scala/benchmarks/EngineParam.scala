package benchmarks

import org.openjdk.jmh.annotations.{Param, Scope, State}
import rescala.Schedulers
import rescala.core.{Scheduler, Struct}
import rescala.interface.RescalaInterface

import scala.collection.mutable

@State(Scope.Benchmark)
class EngineParam[S <: Struct] {
  @Param(Array("synchron", "parrp", "stm", "fullmv"))
  var engineName: String = _

  def engine: RescalaInterface[S] = RescalaInterface.interfaceFor(engineName match {
    case "stm" => rescala.stm.STMScheduler.stm.asInstanceOf[Scheduler[S]]
    case "restoring" => rescala.restoration.RestoringInterface("", mutable.HashMap()).asInstanceOf[Scheduler[S]]
    case "fullmv" => new rescala.fullmv.FullMVEngine(scala.concurrent.duration.Duration.Zero, "benchmark").asInstanceOf[Scheduler[S]]
    case other => Schedulers.byName[S](other)
  })
}
