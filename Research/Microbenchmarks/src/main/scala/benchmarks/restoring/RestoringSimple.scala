package benchmarks.restoring

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Size, Step, Workload}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.engine.Engine
import rescala.propagation.Turn
import rescala.reactives.{Event, Evt}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class RestoringSimple[S <: rescala.graph.Struct] {

  implicit var engine: Engine[S, Turn[S]] = _

  var source: Evt[Int, S] = _
  var result: Event[Int, S] = _

  @Setup
  def setup(params: BenchmarkParams, size: Size, engineParam: EngineParam[S], work: Workload) = {
    engine = engineParam.engine
    source = engine.Evt[Int]
    result = source
    for (i <- Range(1, 100)) {
      if (size.size <= 0 || i % size.size == 0) {
        result = result.count.changed
      }
      else {
        result = result.map{v => val r = v + 1; work.consume(); r}
      }
    }
  }

  @Benchmark
  def run(step: Step): Unit = source.apply(step.run())
}
