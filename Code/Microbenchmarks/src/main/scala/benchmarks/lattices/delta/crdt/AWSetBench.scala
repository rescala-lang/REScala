package benchmarks.lattices.delta.crdt

import kofre.datatypes.AddWinsSet
import kofre.dotted.Dotted
import org.openjdk.jmh.annotations.*

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
    (0 until size).foldLeft(NamedDeltaBuffer.dotted("a", AddWinsSet.empty[Int])) {
      case (s, e) => s.add(using s.replicaID)(e)
    }

  @Setup
  def setup(): Unit = {
    set = createBySize(size)
  }

  @Benchmark
  def elements(): Set[Int] = set.elements

  @Benchmark
  def add(): DeltaBufferDotted[AddWinsSet[Int]] = set.add(using set.replicaID)(-1)

  @Benchmark
  def addAll(): DeltaBufferDotted[AddWinsSet[Int]] =
    val ndb = NamedDeltaBuffer.dotted("a", AddWinsSet.empty[Int])
    ndb.addAll(using ndb.replicaID)(0 until size)

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
