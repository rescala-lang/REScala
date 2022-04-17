package benchmarks.lattices.delta.crdt

import kofre.decompose.interfaces.LWWRegisterInterface.LWWRegister
import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.crdt.reactive.ReactiveDeltaCRDT
import kofre.decompose.interfaces.LWWRegisterInterface.LWWRegisterSyntax

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class LWWRegisterBench {

  var empty: ReactiveDeltaCRDT[LWWRegister[Int]] = _
  var full: ReactiveDeltaCRDT[LWWRegister[Int]]  = _

  @Setup
  def setup(): Unit = {
    empty = ReactiveDeltaCRDT[LWWRegister[Int]]("a")
    full = ReactiveDeltaCRDT[LWWRegister[Int]]("b").write(0)
  }

  @Benchmark
  def readEmpty(): Option[Int] = empty.read

  @Benchmark
  def readFull(): Option[Int] = full.read

  @Benchmark
  def writeEmpty(): ReactiveDeltaCRDT[LWWRegister[Int]] = empty.write(1)

  @Benchmark
  def writeFull(): ReactiveDeltaCRDT[LWWRegister[Int]] = full.write(1)

  @Benchmark
  def mapEmpty(): ReactiveDeltaCRDT[LWWRegister[Int]] = empty.map(_ + 1)

  @Benchmark
  def mapFull(): ReactiveDeltaCRDT[LWWRegister[Int]] = full.map(_ + 1)

  @Benchmark
  def clearEmpty(): ReactiveDeltaCRDT[LWWRegister[Int]] = empty.clear()

  @Benchmark
  def clearFull(): ReactiveDeltaCRDT[LWWRegister[Int]] = full.clear()
}
