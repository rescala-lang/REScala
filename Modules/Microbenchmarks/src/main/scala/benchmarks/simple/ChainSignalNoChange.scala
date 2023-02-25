package benchmarks.simple

import benchmarks.{EngineParam, Size, Workload}
import org.openjdk.jmh.annotations._
import rescala.operator.Interface

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class ChainSignalNoChange {

  var engine: Interface = _
  final lazy val stableEngine  = engine
  import stableEngine._

  var source: Var[Int]    = _
  var result: Signal[Int] = _

  @Setup
  def setup(size: Size, engineParam: EngineParam, work: Workload) = {
    engine = engineParam.engine
    source = Var(0)
    result = source
    for (_ <- Range(0, size.size)) {
      result = result.map { v =>
        val r = v + 1; work.consume(); r
      }
    }
  }

  @Benchmark
  def run(): Unit = source.set(0)
}
