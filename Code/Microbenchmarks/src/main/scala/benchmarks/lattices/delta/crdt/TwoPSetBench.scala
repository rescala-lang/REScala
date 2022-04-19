package benchmarks.lattices.delta.crdt

import kofre.decompose.interfaces.TwoPSetInterface.TwoPSet
import org.openjdk.jmh.annotations._
import kofre.decompose.interfaces.TwoPSetInterface.TwoPSetSyntax
import kofre.decompose.containers.ReactiveDeltaCRDT

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
  var size: Int = _

  var set: ReactiveDeltaCRDT[TwoPSet[Int]] = _

  @Setup
  def setup(): Unit = {
    set = (0 until size).foldLeft(ReactiveDeltaCRDT[TwoPSet[Int]]("a")) {
      case (s, e) => s.insert(e)
    }
  }

  @Benchmark
  def elements(): Set[Int] = set.elements

  @Benchmark
  def insert(): ReactiveDeltaCRDT[TwoPSet[Int]] = set.insert(-1)

  @Benchmark
  def remove(): ReactiveDeltaCRDT[TwoPSet[Int]] = set.remove(0)
}
