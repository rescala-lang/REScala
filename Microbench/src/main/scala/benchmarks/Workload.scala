package benchmarks

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@State(Scope.Benchmark)
class Workload {
  @Param(Array("0" /*, "10000", "100000", "1000000"*/))
  var work: Long = _
  def consume() = Blackhole.consumeCPU(work)
}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 1, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 2, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
class WorkReference {
  @Benchmark
  def reference(work: Workload): Unit = Blackhole.consumeCPU(work.work)
}
