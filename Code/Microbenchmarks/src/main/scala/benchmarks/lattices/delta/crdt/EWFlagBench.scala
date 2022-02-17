package benchmarks.lattices.delta.crdt

import kofre.causality.{CContext, CausalContext}
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

  var flagEnabled: EWFlag[CausalContext]  = _
  var flagDisabled: EWFlag[CausalContext] = _

  @Setup
  def setup(): Unit = {
    flagEnabled = EWFlag("a")(CContext.intTreeCC).enable()
    flagDisabled = EWFlag("b")(CContext.intTreeCC).disable()
  }

  @Benchmark
  def readEnabled(): Boolean = flagEnabled.read

  @Benchmark
  def readDisabled(): Boolean = flagDisabled.read

  @Benchmark
  def enableEnabled(): EWFlag[CausalContext] = flagEnabled.enable()

  @Benchmark
  def enableDisabled(): EWFlag[CausalContext] = flagDisabled.enable()

  @Benchmark
  def disableEnabled(): EWFlag[CausalContext] = flagEnabled.disable()

  @Benchmark
  def disableDisabled(): EWFlag[CausalContext] = flagDisabled.disable()
}
