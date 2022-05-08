package benchmarks.lattices.delta.crdt

import kofre.decompose.containers.ReactiveDeltaCRDT
import kofre.predef.AddWinsSet
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class AWSetBench {

  @Param(Array("0", "1", "10", "100", "1000"))
  var size: Int = _

  var set: ReactiveDeltaCRDT[AddWinsSet[Int]] = _

  def createBySize(size: Int): ReactiveDeltaCRDT[AddWinsSet[Int]] =
    (0 until size).foldLeft(ReactiveDeltaCRDT[AddWinsSet[Int]]("a")) {
      case (s, e) => s.add(e)
    }

  @Setup
  def setup(): Unit = {
    set = createBySize(size)
  }

  @Benchmark
  def elements(): Set[Int] = set.elements

  @Benchmark
  def add(): ReactiveDeltaCRDT[AddWinsSet[Int]] = set.add(-1)

  @Benchmark
  def addAll(): ReactiveDeltaCRDT[AddWinsSet[Int]] = ReactiveDeltaCRDT[AddWinsSet[Int]]("a").addAll(0 until size)

  @Benchmark
  def remove(): ReactiveDeltaCRDT[AddWinsSet[Int]] = set.remove(0)

  @Benchmark
  def removeBy(): ReactiveDeltaCRDT[AddWinsSet[Int]] = set.removeBy((e: Int) => e == 0)

  @Benchmark
  def removeAll(): ReactiveDeltaCRDT[AddWinsSet[Int]] = set.removeAll(set.elements)

  @Benchmark
  def clear(): ReactiveDeltaCRDT[AddWinsSet[Int]] = set.clear()

  @Benchmark
  def construct(): ReactiveDeltaCRDT[AddWinsSet[Int]] = createBySize(size)
}
