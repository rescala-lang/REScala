package benchmarks.lattices.delta.crdt

import kofre.decompose.interfaces.EWFlagInterface.{EWFlag, EWFlagSyntax}
import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.crdt.reactive.ReactiveDeltaCRDT

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class EWFlagBench {

  var flagEnabled: ReactiveDeltaCRDT[EWFlag]  = _
  var flagDisabled: ReactiveDeltaCRDT[EWFlag] = _

  @Setup
  def setup(): Unit = {
    flagEnabled = ReactiveDeltaCRDT[EWFlag]("a").enable()
    flagDisabled = ReactiveDeltaCRDT[EWFlag]("b").disable()
  }

  @Benchmark
  def readEnabled(): Boolean = flagEnabled.read

  @Benchmark
  def readDisabled(): Boolean = flagDisabled.read

  @Benchmark
  def enableEnabled(): ReactiveDeltaCRDT[EWFlag] = flagEnabled.enable()

  @Benchmark
  def enableDisabled(): ReactiveDeltaCRDT[EWFlag] = flagDisabled.enable()

  @Benchmark
  def disableEnabled(): ReactiveDeltaCRDT[EWFlag] = flagEnabled.disable()

  @Benchmark
  def disableDisabled(): ReactiveDeltaCRDT[EWFlag] = flagDisabled.disable()
}
