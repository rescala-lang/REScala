package benchmarks

import org.openjdk.jmh.annotations.{Param, Scope, State}
import rescala.propagation.Turn
import rescala.synchronization.Backoff
import rescala.engines.{Engines, Engine}

@State(Scope.Benchmark)
class EngineParam[S <: rescala.graph.Spores] {
  @Param(Array("synchron", "parrp", "stm"))
  var engineName: String = _

  @Param(Array("100000"))
  var minBackoff: Long = _
  @Param(Array("10000000"))
  var maxBackoff: Long = _
  @Param(Array("1.2"))
  var factorBackoff: Double = _

  def engine: Engine[S, Turn[S]] = {
    if (engineName == "parrp") Engines.spinningWithBackoff(() => new Backoff(minBackoff, maxBackoff, factorBackoff)).asInstanceOf[Engine[S, Turn[S]]]
    else Engines.byName[S](engineName)
  }
}
