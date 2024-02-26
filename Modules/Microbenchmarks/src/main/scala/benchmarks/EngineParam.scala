package benchmarks

import org.openjdk.jmh.annotations.{Param, Scope, State}
import reactives.operator.Interface

@State(Scope.Benchmark)
class EngineParam {
  @Param(Array("synchron", "parrp", "fullmv", "toposort", "sidup"))
  var engineName: String = scala.compiletime.uninitialized

  def engine: Interface = reactives.interfaces.byName(engineName)
}
