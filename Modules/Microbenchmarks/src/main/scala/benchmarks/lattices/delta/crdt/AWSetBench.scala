package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import rdts.datatypes.contextual.ReplicatedSet
import rdts.dotted.Dotted

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
      case (s, e) => s.mod(_.add(using s.replicaID)(e))
    }

  @Setup
  def setup(): Unit = {
    set = createBySize(size)
  }

  @Benchmark
  def elements(): Set[Int] = set.state.data.elements

  @Benchmark
  def add(): DeltaBufferDotted[ReplicatedSet[Int]] = set.mod(_.add(using set.replicaID)(-1))

  @Benchmark
  def addAll(): DeltaBufferDotted[ReplicatedSet[Int]] =
    val ndb = NamedDeltaBuffer.dotted("a", ReplicatedSet.empty[Int])
    ndb.mod(_.addAll(using ndb.replicaID)(0 until size))

  @Benchmark
  def remove(): DeltaBufferDotted[ReplicatedSet[Int]] = set.mod(_.remove(0))

  @Benchmark
  def removeBy(): DeltaBufferDotted[ReplicatedSet[Int]] = set.mod(_.removeBy((e: Int) => e == 0))

  @Benchmark
  def removeAll(): DeltaBufferDotted[ReplicatedSet[Int]] = set.mod(_.removeAll(set.state.data.elements))

  @Benchmark
  def clear(): DeltaBufferDotted[ReplicatedSet[Int]] = set.mod(_.clear())

  @Benchmark
  def construct(): DeltaBufferDotted[ReplicatedSet[Int]] = createBySize(size)
}
