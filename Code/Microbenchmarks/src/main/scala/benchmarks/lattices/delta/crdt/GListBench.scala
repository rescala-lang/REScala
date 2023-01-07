package benchmarks.lattices.delta.crdt

import kofre.datatypes.GrowOnlyList
import org.openjdk.jmh.annotations._
import kofre.deprecated.containers.DeltaBufferRDT

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class GListBench {

  @Param(Array("0", "1", "10", "100", "1000"))
  var listSize: Int = _

  var list: DeltaBufferRDT[GrowOnlyList[Int]] = _

  @Setup
  def setup(): Unit = {
    list = (0 until listSize).foldLeft(DeltaBufferRDT("a", GrowOnlyList.empty[Int])) {
      case (c, i) => c.insertGL(0, i)
    }
  }

  @Benchmark
  def toList: List[Int] = list.toList

  @Benchmark
  def size(): Int = list.size

  @Benchmark
  def readFirst(): Option[Int] = list.read(0)

  @Benchmark
  def readLast(): Option[Int] = list.read(listSize - 1)

  @Benchmark
  def insertStart(): DeltaBufferRDT[GrowOnlyList[Int]] = list.insertGL(0, -1)

  @Benchmark
  def insertEnd(): DeltaBufferRDT[GrowOnlyList[Int]] = list.insertGL(listSize, -1)
}
