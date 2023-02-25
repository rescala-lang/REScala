package benchmarks

import org.openjdk.jmh.annotations.{Param, Scope, State}
import rescala.operator.Interface

@State(Scope.Benchmark)
class EngineParam {
  @Param(Array("synchron", "parrp", "fullmv", "toposort", "sidup"))
  var engineName: String = _

  def engine: Interface = rescala.interfaces.byName(engineName)
}
