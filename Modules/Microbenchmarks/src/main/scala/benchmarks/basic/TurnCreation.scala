package benchmarks.basic

import benchmarks.EngineParam
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class TurnCreation {

  var engine: reactives.default.type = scala.compiletime.uninitialized
  final lazy val stableEngine        = engine

  @Setup
  def setup(engineParam: EngineParam) = {
    engine = engineParam.engine
  }

  @Benchmark
  def run(): Any = {
    engine.transaction()(identity)
  }

}
