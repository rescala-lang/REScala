package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.CContext.DietMapCContext
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

  var flagEnabled: EWFlag[DietMapCContext]  = _
  var flagDisabled: EWFlag[DietMapCContext] = _

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
  def enableEnabled(): EWFlag[DietMapCContext] = flagEnabled.enable()

  @Benchmark
  def enableDisabled(): EWFlag[DietMapCContext] = flagDisabled.enable()

  @Benchmark
  def disableEnabled(): EWFlag[DietMapCContext] = flagEnabled.disable()

  @Benchmark
  def disableDisabled(): EWFlag[DietMapCContext] = flagDisabled.disable()
}
