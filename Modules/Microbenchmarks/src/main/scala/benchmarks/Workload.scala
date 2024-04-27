package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
class Workload {
  @Param(Array("0" /*, "10000", "100000", "1000000"*/ ))
  var work: Long = scala.compiletime.uninitialized
  @Param(Array("0"))
  var workSecondary: Long      = scala.compiletime.uninitialized
  def consume(): Unit          = Blackhole.consumeCPU(work)
  def consumeSecondary(): Unit = Blackhole.consumeCPU(workSecondary)
}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 2, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 2, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
class WorkReference {
  @Benchmark
  def reference(work: Workload): Unit = work.consume()

  @Benchmark
  def allocationStress(): Object = {
    new Object()
  }
}
