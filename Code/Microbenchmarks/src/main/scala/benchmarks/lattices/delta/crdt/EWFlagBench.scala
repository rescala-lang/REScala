package benchmarks.lattices.delta.crdt

import kofre.decompose.interfaces.EnableWinsFlag.{EWFlagPlain, EnableWinsFlagOps}
import org.openjdk.jmh.annotations._
import kofre.decompose.containers.ReactiveDeltaCRDT

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class EWFlagBench {

  var flagEnabled: ReactiveDeltaCRDT[EWFlagPlain] = _
  var flagDisabled: ReactiveDeltaCRDT[EWFlagPlain]     = _

  @Setup
  def setup(): Unit = {
    flagEnabled = ReactiveDeltaCRDT[EWFlagPlain]("a").enable()
    flagDisabled = ReactiveDeltaCRDT[EWFlagPlain]("b").disable()
  }

  @Benchmark
  def readEnabled(): Boolean = flagEnabled.read

  @Benchmark
  def readDisabled(): Boolean = flagDisabled.read

  @Benchmark
  def enableEnabled(): ReactiveDeltaCRDT[EWFlagPlain] = flagEnabled.enable()

  @Benchmark
  def enableDisabled(): ReactiveDeltaCRDT[EWFlagPlain] = flagDisabled.enable()

  @Benchmark
  def disableEnabled(): ReactiveDeltaCRDT[EWFlagPlain] = flagEnabled.disable()

  @Benchmark
  def disableDisabled(): ReactiveDeltaCRDT[EWFlagPlain] = flagDisabled.disable()
}
