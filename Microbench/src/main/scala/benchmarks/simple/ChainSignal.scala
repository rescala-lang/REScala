package benchmarks.simple

import java.util.concurrent.TimeUnit

import benchmarks.{Size, EngineParam, Step}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.turns.{Engine, Turn}
import rescala.{Signal, Signals, Var}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class ChainSignal[S <: rescala.graph.Spores] {

  implicit var engine: Engine[S, Turn[S]] = _

  var source: Var[Int, S] = _
  var result: Signal[Int, S] = _

  @Setup
  def setup(params: BenchmarkParams, size: Size, step: Step, engineParam: EngineParam[S]) = {
    engine = engineParam.engine
    source = Var(step.run())
    result = source
    for (_ <- Range(0, size.size)) {
      result = result.map(_ + 1)
    }
  }

  @Benchmark
  def run(step: Step): Unit = source.set(step.run())
}
