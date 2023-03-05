package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*

import kofre.base.Uid.asId
import kofre.datatypes.LastWriterWins

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class LWWRegisterBench {

  var empty: DeltaBufferDotted[LastWriterWins[Int]] = _
  var full: DeltaBufferDotted[LastWriterWins[Int]]  = _

  @Setup
  def setup(): Unit = {
    empty = NamedDeltaBuffer.dotted("a", LastWriterWins.empty[Int])
    full = NamedDeltaBuffer.dotted("b", LastWriterWins.empty[Int]).write(using "b".asId)(0)
  }

  @Benchmark
  def readEmpty(): Option[Int] = empty.read

  @Benchmark
  def readFull(): Option[Int] = full.read

  @Benchmark
  def writeEmpty(): DeltaBufferDotted[LastWriterWins[Int]] = empty.write(using empty.replicaID)(1)

  @Benchmark
  def writeFull(): DeltaBufferDotted[LastWriterWins[Int]] = full.write(using full.replicaID)(1)

  @Benchmark
  def mapEmpty(): DeltaBufferDotted[LastWriterWins[Int]] = empty.map(using empty.replicaID)(_ + 1)

  @Benchmark
  def mapFull(): DeltaBufferDotted[LastWriterWins[Int]] = full.map(using full.replicaID)(_ + 1)

  @Benchmark
  def clearEmpty(): DeltaBufferDotted[LastWriterWins[Int]] = empty.clear()

  @Benchmark
  def clearFull(): DeltaBufferDotted[LastWriterWins[Int]] = full.clear()
}
