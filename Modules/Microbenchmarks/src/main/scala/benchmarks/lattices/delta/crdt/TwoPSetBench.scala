package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import rdts.base.LocalUid.asId
import rdts.datatypes.TwoPhaseSet

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class TwoPSetBench {

  @Param(Array("0", "1", "10", "100", "1000"))
  var size: Int = scala.compiletime.uninitialized

  var set: NamedDeltaBuffer[TwoPhaseSet[Int]] = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    set = (0 until size).foldLeft(NamedDeltaBuffer("a".asId, TwoPhaseSet.empty[Int])) {
      case (s, e) => s.mod(_.insert(e))
    }
  }

  @Benchmark
  def elements(): Set[Int] = set.state.elements

  @Benchmark
  def insert(): NamedDeltaBuffer[TwoPhaseSet[Int]] = set.mod(_.insert(-1))

  @Benchmark
  def remove(): NamedDeltaBuffer[TwoPhaseSet[Int]] = set.mod(_.remove(0))
}
