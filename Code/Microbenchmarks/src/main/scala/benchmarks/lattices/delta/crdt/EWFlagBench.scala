package benchmarks.lattices.delta.crdt

import kofre.datatypes.EnableWinsFlag
import kofre.deprecated.containers.DeltaBufferRDT
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class EWFlagBench {

  var flagEnabled: DeltaBufferRDT[EnableWinsFlag]  = _
  var flagDisabled: DeltaBufferRDT[EnableWinsFlag] = _

  @Setup
  def setup(): Unit = {
    flagEnabled = DeltaBufferRDT.empty("a", EnableWinsFlag.empty).enable()
    flagDisabled = DeltaBufferRDT.empty("b", EnableWinsFlag.empty).disable()
  }

  @Benchmark
  def readEnabled(): Boolean = flagEnabled.read

  @Benchmark
  def readDisabled(): Boolean = flagDisabled.read

  @Benchmark
  def enableEnabled(): DeltaBufferRDT[EnableWinsFlag] = flagEnabled.enable()

  @Benchmark
  def enableDisabled(): DeltaBufferRDT[EnableWinsFlag] = flagDisabled.enable()

  @Benchmark
  def disableEnabled(): DeltaBufferRDT[EnableWinsFlag] = flagEnabled.disable()

  @Benchmark
  def disableDisabled(): DeltaBufferRDT[EnableWinsFlag] = flagDisabled.disable()
}
