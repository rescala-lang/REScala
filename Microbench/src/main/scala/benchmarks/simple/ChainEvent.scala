package benchmarks.simple

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Size, Step}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.turns.{Engine, Turn}
import rescala.{Event, Evt}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Thread)
class ChainEvent[S <: rescala.graph.Spores] {

  implicit var engine: Engine[S, Turn[S]] = _

  var source: Evt[Int, S] = _
  var result: Event[Int, S] = _

  @Setup
  def setup(params: BenchmarkParams, size: Size, engineParam: EngineParam[S]) = {
    engine = engineParam.engine
    source = engine.Evt[Int]()
    result = source
    for (_ <- Range(0, size.size)) {
      result = result.map(_ + 1)
    }
  }

  @Benchmark
  def run(step: Step): Unit = source.apply(step.run())
}
