package benchmarks.lattices.delta.crdt

import rdts.datatypes.contextual.ReplicatedSet
import rdts.dotted.Dotted
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
  var size: Int = scala.compiletime.uninitialized

  var set: DeltaBufferDotted[ReplicatedSet[Int]] = scala.compiletime.uninitialized

  def createBySize(size: Int): DeltaBufferDotted[ReplicatedSet[Int]] =
    (0 until size).foldLeft(NamedDeltaBuffer.dotted("a", ReplicatedSet.empty[Int])) {
      case (s, e) => s.add(using s.replicaID)(e)
    }

  @Setup
  def setup(): Unit = {
    set = createBySize(size)
  }

  @Benchmark
  def elements(): Set[Int] = set.elements

  @Benchmark
  def add(): DeltaBufferDotted[ReplicatedSet[Int]] = set.add(using set.replicaID)(-1)

  @Benchmark
  def addAll(): DeltaBufferDotted[ReplicatedSet[Int]] =
    val ndb = NamedDeltaBuffer.dotted("a", ReplicatedSet.empty[Int])
    ndb.addAll(using ndb.replicaID)(0 until size)

  @Benchmark
  def remove(): DeltaBufferDotted[ReplicatedSet[Int]] = set.remove(0)

  @Benchmark
  def removeBy(): DeltaBufferDotted[ReplicatedSet[Int]] = set.removeBy((e: Int) => e == 0)

  @Benchmark
  def removeAll(): DeltaBufferDotted[ReplicatedSet[Int]] = set.removeAll(set.elements)

  @Benchmark
  def clear(): DeltaBufferDotted[ReplicatedSet[Int]] = set.clear()

  @Benchmark
  def construct(): DeltaBufferDotted[ReplicatedSet[Int]] = createBySize(size)
}
