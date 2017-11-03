package benchmarks

import org.openjdk.jmh.annotations.{Param, Scope, State}
import rescala.Engines
import rescala.core.{Engine, Struct}

@State(Scope.Benchmark)
class EngineParam[S <: Struct] {
  @Param(Array("synchron", "parrp", "locksweep", "stm", "fullmv"))
  var engineName: String = _

  def engine: Engine[S] = engineName match {
    case "stm" => rescala.stm.STMEngine.stm.asInstanceOf[Engine[S]]
    case "restoring" => new rescala.restore.ReStoringEngine().asInstanceOf[Engine[S]]
    case "fullmv" => rescala.fullmv.FullMVEngine.default.asInstanceOf[Engine[S]]
    case other => Engines.byName[S](other)
  }
}
