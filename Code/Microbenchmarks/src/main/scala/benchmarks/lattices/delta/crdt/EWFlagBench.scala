package benchmarks.lattices.delta.crdt

import kofre.decompose.interfaces.EnableWinsFlag.{EWFlagPlain, EnableWinsFlagOps}
import org.openjdk.jmh.annotations._
import kofre.decompose.containers.DeltaBufferRDT

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class EWFlagBench {

  var flagEnabled: DeltaBufferRDT[EWFlagPlain]  = _
  var flagDisabled: DeltaBufferRDT[EWFlagPlain] = _

  @Setup
  def setup(): Unit = {
    flagEnabled = DeltaBufferRDT[EWFlagPlain]("a").enable()
    flagDisabled = DeltaBufferRDT[EWFlagPlain]("b").disable()
  }

  @Benchmark
  def readEnabled(): Boolean = flagEnabled.read

  @Benchmark
  def readDisabled(): Boolean = flagDisabled.read

  @Benchmark
  def enableEnabled(): DeltaBufferRDT[EWFlagPlain] = flagEnabled.enable()

  @Benchmark
  def enableDisabled(): DeltaBufferRDT[EWFlagPlain] = flagDisabled.enable()

  @Benchmark
  def disableEnabled(): DeltaBufferRDT[EWFlagPlain] = flagEnabled.disable()

  @Benchmark
  def disableDisabled(): DeltaBufferRDT[EWFlagPlain] = flagDisabled.disable()
}
