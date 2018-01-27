package benchmarks

import org.openjdk.jmh.annotations.{Param, Scope, State}
import rescala.Engines
import rescala.core.{Scheduler, Struct}

@State(Scope.Benchmark)
class EngineParam[S <: Struct] {
  @Param(Array("synchron", "parrp", "locksweep", "stm", "fullmv"))
  var engineName: String = _

  def engine: Scheduler[S] = engineName match {
    case "stm" => rescala.stm.STMEngine.stm.asInstanceOf[Scheduler[S]]
    case "restoring" => new rescala.restoration.ReStoringScheduler().asInstanceOf[Scheduler[S]]
    case "fullmv" => new rescala.fullmv.FullMVEngine(scala.concurrent.duration.Duration.Zero, "benchmark").asInstanceOf[Scheduler[S]]
    case other => Engines.byName[S](other)
  }
}
