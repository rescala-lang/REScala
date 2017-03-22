package benchmarks

import org.openjdk.jmh.annotations.{Param, Scope, State}
import rescala.Engines
import rescala.engine.Engine
import rescala.parrp.Backoff
import rescala.propagation.Turn

@State(Scope.Benchmark)
class EngineParam[S <: rescala.graph.Struct] {
  @Param(Array("synchron", "parrp", "locksweep", "stm"))
  var engineName: String = _

  @Param(Array("100000"))
  var minBackoff: Long = _
  @Param(Array("10000000"))
  var maxBackoff: Long = _
  @Param(Array("1.2"))
  var factorBackoff: Double = _

  def engine: Engine[S, Turn[S]] = engineName match {
    case "parrp" => Engines.parrpWithBackoff(() => new Backoff(minBackoff, maxBackoff, factorBackoff)).asInstanceOf[Engine[S, Turn[S]]]
    case "locksweep" => Engines.locksweepWithBackoff(() => new Backoff(minBackoff, maxBackoff, factorBackoff)).asInstanceOf[Engine[S, Turn[S]]]
    case "stm" => rescala.stm.STMEngine.stm.asInstanceOf[Engine[S, Turn[S]]]
    case "restoring" => new rescala.restore.ReStoringEngine().asInstanceOf[Engine[S, Turn[S]]]
    case other => Engines.byName[S](other)
  }
}
