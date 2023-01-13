package benchmarks.lattices.delta.crdt

import kofre.datatypes.AddWinsSet
import kofre.deprecated.containers.DeltaBufferDotted
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

  var set: DeltaBufferDotted[AddWinsSet[Int]] = _

  def createBySize(size: Int): DeltaBufferDotted[AddWinsSet[Int]] =
    (0 until size).foldLeft(DeltaBufferDotted.empty("a", AddWinsSet.empty[Int])) {
      case (s, e) => s.add(e)
    }

  @Setup
  def setup(): Unit = {
    set = createBySize(size)
  }

  @Benchmark
  def elements(): Set[Int] = set.elements

  @Benchmark
  def add(): DeltaBufferDotted[AddWinsSet[Int]] = set.add(-1)

  @Benchmark
  def addAll(): DeltaBufferDotted[AddWinsSet[Int]] = DeltaBufferDotted.empty("a", AddWinsSet.empty[Int]).addAll(0 until size)

  @Benchmark
  def remove(): DeltaBufferDotted[AddWinsSet[Int]] = set.remove(0)

  @Benchmark
  def removeBy(): DeltaBufferDotted[AddWinsSet[Int]] = set.removeBy((e: Int) => e == 0)

  @Benchmark
  def removeAll(): DeltaBufferDotted[AddWinsSet[Int]] = set.removeAll(set.elements)

  @Benchmark
  def clear(): DeltaBufferDotted[AddWinsSet[Int]] = set.clear()

  @Benchmark
  def construct(): DeltaBufferDotted[AddWinsSet[Int]] = createBySize(size)
}
