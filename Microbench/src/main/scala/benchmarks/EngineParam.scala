package benchmarks

import org.openjdk.jmh.annotations.{Param, Scope, State}
import rescala.synchronization.Engines
import rescala.turns.{Engine, Turn}

@State(Scope.Benchmark)
class EngineParam[S <: rescala.graph.Spores] {
  @Param(Array("synchron", "parrp", "stm", "fair"))
  var engineName: String = _

  @Param(Array("7"))
  var spinningBackOff: Int = _

  def engine: Engine[S, Turn[S]] = {
    if (engineName == "parrp") Engines.spinningWithBackoff(spinningBackOff).asInstanceOf[Engine[S, Turn[S]]]
    else Engines.byName[S](engineName)
  }
}
