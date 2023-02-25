package benchmarks

import org.openjdk.jmh.annotations.{Param, Scope, State}
import rescala.Schedulers
import rescala.operator.Interface

@State(Scope.Benchmark)
class EngineParam {
  @Param(Array("synchron", "parrp", "fullmv", "toposort", "sidup"))
  var engineName: String = _

  def engine: Interface = Schedulers.byName(engineName)
}
