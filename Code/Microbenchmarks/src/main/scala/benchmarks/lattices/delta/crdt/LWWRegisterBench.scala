package benchmarks.lattices.delta.crdt

import kofre.datatypes.CausalLastWriterWins
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

  var empty: DeltaBufferRDT[CausalLastWriterWins[Int]] = _
  var full: DeltaBufferRDT[CausalLastWriterWins[Int]]  = _

  @Setup
  def setup(): Unit = {
    empty = DeltaBufferRDT("a", CausalLastWriterWins.empty[Int])
    full = DeltaBufferRDT("b", CausalLastWriterWins.empty[Int]).write(0)
  }

  @Benchmark
  def readEmpty(): Option[Int] = empty.read

  @Benchmark
  def readFull(): Option[Int] = full.read

  @Benchmark
  def writeEmpty(): DeltaBufferRDT[CausalLastWriterWins[Int]] = empty.write(1)

  @Benchmark
  def writeFull(): DeltaBufferRDT[CausalLastWriterWins[Int]] = full.write(1)

  @Benchmark
  def mapEmpty(): DeltaBufferRDT[CausalLastWriterWins[Int]] = empty.map(_ + 1)

  @Benchmark
  def mapFull(): DeltaBufferRDT[CausalLastWriterWins[Int]] = full.map(_ + 1)

  @Benchmark
  def clearEmpty(): DeltaBufferRDT[CausalLastWriterWins[Int]] = empty.clear()

  @Benchmark
  def clearFull(): DeltaBufferRDT[CausalLastWriterWins[Int]] = full.clear()
}
