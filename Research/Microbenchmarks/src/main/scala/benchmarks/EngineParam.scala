package benchmarks

import org.openjdk.jmh.annotations.{Param, Scope, State}
import rescala.Engines
import rescala.engine.Engine

@State(Scope.Benchmark)
class EngineParam[S <: rescala.graph.Struct] {
  @Param(Array("synchron", "parrp", "locksweep", "stm"))
  var engineName: String = _

  def engine: Engine[S] = engineName match {
    case "stm" => rescala.stm.STMEngine.stm.asInstanceOf[Engine[S]]
    case "restoring" => new rescala.restore.ReStoringEngine().asInstanceOf[Engine[S]]
    case other => Engines.byName[S](other)
  }
}
