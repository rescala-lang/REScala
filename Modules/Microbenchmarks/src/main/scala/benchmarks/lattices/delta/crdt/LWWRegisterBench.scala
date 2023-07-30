package benchmarks.lattices.delta.crdt

import kofre.datatypes.LastWriterWins
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

  var full: NamedDeltaBuffer[LastWriterWins[Int]]  = _

  @Setup
  def setup(): Unit = {
    full = NamedDeltaBuffer("b", LastWriterWins.now(0))
  }

  @Benchmark
  def readFull(): Int = full.read

  @Benchmark
  def writeFull(): NamedDeltaBuffer[LastWriterWins[Int]] = full.write(1)

  @Benchmark
  def mapFull(): NamedDeltaBuffer[LastWriterWins[Int]] = full.write(full.read + 1)

}
