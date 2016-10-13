package benchmarks

import org.openjdk.jmh.annotations.{Param, Scope, State}
import rescala.engines.{Engine, Engines}
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

  def engine: Engine[S, Turn[S]] = {
    if (engineName == "parrp") Engines.parrpWithBackoff(() => new Backoff(minBackoff, maxBackoff, factorBackoff)).asInstanceOf[Engine[S, Turn[S]]]
    if (engineName == "locksweep") Engines.locksweepWithBackoff(() => new Backoff(minBackoff, maxBackoff, factorBackoff)).asInstanceOf[Engine[S, Turn[S]]]
    else Engines.byName[S](engineName)
  }
}
