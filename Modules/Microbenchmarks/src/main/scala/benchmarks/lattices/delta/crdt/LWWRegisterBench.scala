package benchmarks.lattices.delta.crdt

import kofre.datatypes.CausalLastWriterWins
import org.openjdk.jmh.annotations.*

import kofre.base.Uid.asId

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class LWWRegisterBench {

  var empty: DeltaBufferDotted[CausalLastWriterWins[Int]] = _
  var full: DeltaBufferDotted[CausalLastWriterWins[Int]]  = _

  @Setup
  def setup(): Unit = {
    empty = NamedDeltaBuffer.dotted("a", CausalLastWriterWins.empty[Int])
    full = NamedDeltaBuffer.dotted("b", CausalLastWriterWins.empty[Int]).write(using "b".asId)(0)
  }

  @Benchmark
  def readEmpty(): Option[Int] = empty.read

  @Benchmark
  def readFull(): Option[Int] = full.read

  @Benchmark
  def writeEmpty(): DeltaBufferDotted[CausalLastWriterWins[Int]] = empty.write(using empty.replicaID)(1)

  @Benchmark
  def writeFull(): DeltaBufferDotted[CausalLastWriterWins[Int]] = full.write(using full.replicaID)(1)

  @Benchmark
  def mapEmpty(): DeltaBufferDotted[CausalLastWriterWins[Int]] = empty.map(using empty.replicaID)(_ + 1)

  @Benchmark
  def mapFull(): DeltaBufferDotted[CausalLastWriterWins[Int]] = full.map(using full.replicaID)(_ + 1)

  @Benchmark
  def clearEmpty(): DeltaBufferDotted[CausalLastWriterWins[Int]] = empty.clear()

  @Benchmark
  def clearFull(): DeltaBufferDotted[CausalLastWriterWins[Int]] = full.clear()
}
