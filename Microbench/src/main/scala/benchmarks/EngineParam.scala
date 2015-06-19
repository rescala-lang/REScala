package benchmarks

import org.openjdk.jmh.annotations.{Param, Scope, State}
import rescala.turns.{Engine, Engines, Turn}

@State(Scope.Benchmark)
class EngineParam {
  @Param(Array("synchron", "parrp", "stm"))
  var engineName: String = _

  @Param(Array("7"))
  var spinningBackOff: Int = _

  def engine: Engine[Turn] = {
    if (engineName == "parrp") Engines.spinningWithBackoff(spinningBackOff)
    else Engines.byName(engineName)
  }
}
