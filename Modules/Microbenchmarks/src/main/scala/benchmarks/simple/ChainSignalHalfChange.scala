package benchmarks.simple

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Size, Step, Workload}
import org.openjdk.jmh.annotations._
import rescala.operator.Interface

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class ChainSignalHalfChange {

  var engine: Interface       = scala.compiletime.uninitialized
  final lazy val stableEngine = engine
  import stableEngine._

  var source: Var[Int]    = scala.compiletime.uninitialized
  var result: Signal[Int] = scala.compiletime.uninitialized

  @Setup
  def setup(size: Size, step: Step, engineParam: EngineParam, work: Workload) = {
    engine = engineParam.engine
    source = Var(step.run())
    result = source
    for (_ <- Range(0, size.size)) {
      result = result.map { v =>
        val r = v + 1; work.consume(); r
      }
    }
    for (_ <- Range(0, size.size)) {
      result = result.map { v =>
        v + 1; work.consume(); 0
      }
    }
  }

  @Benchmark
  def run(step: Step): Unit = source.set(step.run())
}
