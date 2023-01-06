package benchmarks.lattices.delta.crdt

import kofre.datatypes.AddWinsSet
import kofre.deprecated.containers.DeltaBufferRDT
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

  var set: DeltaBufferRDT[AddWinsSet[Int]] = _

  def createBySize(size: Int): DeltaBufferRDT[AddWinsSet[Int]] =
    (0 until size).foldLeft(DeltaBufferRDT.empty("a", AddWinsSet.empty[Int])) {
      case (s, e) => s.add(e)
    }

  @Setup
  def setup(): Unit = {
    set = createBySize(size)
  }

  @Benchmark
  def elements(): Set[Int] = set.elements

  @Benchmark
  def add(): DeltaBufferRDT[AddWinsSet[Int]] = set.add(-1)

  @Benchmark
  def addAll(): DeltaBufferRDT[AddWinsSet[Int]] = DeltaBufferRDT.empty("a", AddWinsSet.empty[Int]).addAll(0 until size)

  @Benchmark
  def remove(): DeltaBufferRDT[AddWinsSet[Int]] = set.remove(0)

  @Benchmark
  def removeBy(): DeltaBufferRDT[AddWinsSet[Int]] = set.removeBy((e: Int) => e == 0)

  @Benchmark
  def removeAll(): DeltaBufferRDT[AddWinsSet[Int]] = set.removeAll(set.elements)

  @Benchmark
  def clear(): DeltaBufferRDT[AddWinsSet[Int]] = set.clear()

  @Benchmark
  def construct(): DeltaBufferRDT[AddWinsSet[Int]] = createBySize(size)
}
