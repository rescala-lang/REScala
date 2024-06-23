package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import rdts.base.LocalUid.asId
import rdts.datatypes.LastWriterWins

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class LWWRegisterBench {

  var full: NamedDeltaBuffer[LastWriterWins[Int]] = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    full = NamedDeltaBuffer("b".asId, LastWriterWins.now(0))
  }

  @Benchmark
  def readFull(): Int = full.state.read

  @Benchmark
  def writeFull(): NamedDeltaBuffer[LastWriterWins[Int]] = full.transform(_.write(1))

  @Benchmark
  def mapFull(): NamedDeltaBuffer[LastWriterWins[Int]] = full.transform(_.write(full.state.read + 1))

}
