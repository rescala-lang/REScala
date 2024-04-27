package benchmarks.simple

import benchmarks.Workload
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class PlainWork {
  var work: Workload = scala.compiletime.uninitialized
  @Setup
  def setup(work: Workload): Unit = this.work = work

  @Benchmark
  def run(): Unit = work.consume()
}
