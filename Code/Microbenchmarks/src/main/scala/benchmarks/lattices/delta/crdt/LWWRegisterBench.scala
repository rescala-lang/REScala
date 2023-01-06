package benchmarks.lattices.delta.crdt

import kofre.decompose.interfaces.CausalLastWriterWinsRegister
import kofre.deprecated.containers.DeltaBufferRDT
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class LWWRegisterBench {

  var empty: DeltaBufferRDT[CausalLastWriterWinsRegister[Int]] = _
  var full: DeltaBufferRDT[CausalLastWriterWinsRegister[Int]]  = _

  @Setup
  def setup(): Unit = {
    empty = DeltaBufferRDT("a", CausalLastWriterWinsRegister.empty[Int])
    full = DeltaBufferRDT("b", CausalLastWriterWinsRegister.empty[Int]).write(0)
  }

  @Benchmark
  def readEmpty(): Option[Int] = empty.read

  @Benchmark
  def readFull(): Option[Int] = full.read

  @Benchmark
  def writeEmpty(): DeltaBufferRDT[CausalLastWriterWinsRegister[Int]] = empty.write(1)

  @Benchmark
  def writeFull(): DeltaBufferRDT[CausalLastWriterWinsRegister[Int]] = full.write(1)

  @Benchmark
  def mapEmpty(): DeltaBufferRDT[CausalLastWriterWinsRegister[Int]] = empty.map(_ + 1)

  @Benchmark
  def mapFull(): DeltaBufferRDT[CausalLastWriterWinsRegister[Int]] = full.map(_ + 1)

  @Benchmark
  def clearEmpty(): DeltaBufferRDT[CausalLastWriterWinsRegister[Int]] = empty.clear()

  @Benchmark
  def clearFull(): DeltaBufferRDT[CausalLastWriterWinsRegister[Int]] = full.clear()
}
