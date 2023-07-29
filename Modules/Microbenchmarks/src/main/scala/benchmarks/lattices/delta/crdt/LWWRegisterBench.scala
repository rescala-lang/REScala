package benchmarks.lattices.delta.crdt

import kofre.datatypes.alternatives.lww.CausalLastWriterWins
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

  var full: DeltaBufferDotted[CausalLastWriterWins[Int]]  = _

  @Setup
  def setup(): Unit = {
    full = NamedDeltaBuffer.dottedInit("b", CausalLastWriterWins.now(_, 0))
  }

  @Benchmark
  def readFull(): Int = full.read

  @Benchmark
  def writeFull(): DeltaBufferDotted[CausalLastWriterWins[Int]] = full.write(using full.replicaID)(1)

  @Benchmark
  def mapFull(): DeltaBufferDotted[CausalLastWriterWins[Int]] = full.write(using full.replicaID)(full.read + 1)

}
