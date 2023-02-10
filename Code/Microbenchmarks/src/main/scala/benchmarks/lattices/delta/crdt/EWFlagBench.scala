package benchmarks.lattices.delta.crdt

import kofre.datatypes.EnableWinsFlag
import org.openjdk.jmh.annotations.*
import kofre.base.Id.asId

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class EWFlagBench {

  var flagEnabled: DeltaBufferDotted[EnableWinsFlag]  = _
  var flagDisabled: DeltaBufferDotted[EnableWinsFlag] = _

  @Setup
  def setup(): Unit = {
    flagEnabled = NamedDeltaBuffer.dotted("a", EnableWinsFlag.empty).enable(using "a".asId)()
    flagDisabled = NamedDeltaBuffer.dotted("b", EnableWinsFlag.empty).disable()
  }

  @Benchmark
  def readEnabled(): Boolean = flagEnabled.read

  @Benchmark
  def readDisabled(): Boolean = flagDisabled.read

  @Benchmark
  def enableEnabled(): DeltaBufferDotted[EnableWinsFlag] = flagEnabled.enable(using flagEnabled.replicaID)()

  @Benchmark
  def enableDisabled(): DeltaBufferDotted[EnableWinsFlag] = flagDisabled.enable(using flagDisabled.replicaID)()

  @Benchmark
  def disableEnabled(): DeltaBufferDotted[EnableWinsFlag] = flagEnabled.disable()

  @Benchmark
  def disableDisabled(): DeltaBufferDotted[EnableWinsFlag] = flagDisabled.disable()
}
