package benchmarks.lattices.delta.crdt

import kofre.decompose.interfaces.LWWRegisterInterface.{LWWRegister, LWWRegisterSyntax}
import org.openjdk.jmh.annotations._
import kofre.decompose.interfaces.LWWRegisterInterface
import kofre.deprecated.containers.DeltaBufferRDT

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class LWWRegisterBench {

  var empty: DeltaBufferRDT[LWWRegister[Int]] = _
  var full: DeltaBufferRDT[LWWRegister[Int]]  = _

  @Setup
  def setup(): Unit = {
    empty = DeltaBufferRDT("a", LWWRegisterInterface.empty[Int])
    full = DeltaBufferRDT("b", LWWRegisterInterface.empty[Int]).write(0)
  }

  @Benchmark
  def readEmpty(): Option[Int] = empty.read

  @Benchmark
  def readFull(): Option[Int] = full.read

  @Benchmark
  def writeEmpty(): DeltaBufferRDT[LWWRegister[Int]] = empty.write(1)

  @Benchmark
  def writeFull(): DeltaBufferRDT[LWWRegister[Int]] = full.write(1)

  @Benchmark
  def mapEmpty(): DeltaBufferRDT[LWWRegister[Int]] = empty.map(_ + 1)

  @Benchmark
  def mapFull(): DeltaBufferRDT[LWWRegister[Int]] = full.map(_ + 1)

  @Benchmark
  def clearEmpty(): DeltaBufferRDT[LWWRegister[Int]] = empty.clear()

  @Benchmark
  def clearFull(): DeltaBufferRDT[LWWRegister[Int]] = full.clear()
}
