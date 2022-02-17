package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.crdt.reactive.EWFlag

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class EWFlagBench {

  var flagEnabled: EWFlag  = _
  var flagDisabled: EWFlag = _

  @Setup
  def setup(): Unit = {
    flagEnabled = EWFlag("a").enable()
    flagDisabled = EWFlag("b").disable()
  }

  @Benchmark
  def readEnabled(): Boolean = flagEnabled.read

  @Benchmark
  def readDisabled(): Boolean = flagDisabled.read

  @Benchmark
  def enableEnabled(): EWFlag = flagEnabled.enable()

  @Benchmark
  def enableDisabled(): EWFlag = flagDisabled.enable()

  @Benchmark
  def disableEnabled(): EWFlag = flagEnabled.disable()

  @Benchmark
  def disableDisabled(): EWFlag = flagDisabled.disable()
}
