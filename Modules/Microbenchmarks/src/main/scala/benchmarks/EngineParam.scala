package benchmarks

import org.openjdk.jmh.annotations.{Param, Scope, State}
import reactives.core.Scheduler
import reactives.operator.Interface

case class Hack(engine: Scheduler[Interface.State])

@State(Scope.Benchmark)
class EngineParam {
  @Param(Array("synchron", "parrp", "fullmv", "toposort", "sidup"))
  var engineName: String = scala.compiletime.uninitialized

  def engine: Interface = reactives.default
}
