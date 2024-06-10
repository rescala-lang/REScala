package benchmarks

import org.openjdk.jmh.annotations.{Param, Scope, State}
import reactives.core.Scheduler

@State(Scope.Benchmark)
class EngineParam {
  @Param(Array("synchron", "parrp", "fullmv", "toposort", "sidup"))
  var engineName: String = scala.compiletime.uninitialized

  def engine: reactives.default.type = reactives.default
}
