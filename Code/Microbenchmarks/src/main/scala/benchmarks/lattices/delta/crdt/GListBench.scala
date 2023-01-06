package benchmarks.lattices.delta.crdt

import kofre.decompose.interfaces.GListInterface.GList
import org.openjdk.jmh.annotations._
import kofre.decompose.interfaces.GListInterface
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

  var list: DeltaBufferRDT[GList[Int]] = _

  @Setup
  def setup(): Unit = {
    list = (0 until listSize).foldLeft(DeltaBufferRDT("a", GListInterface.empty[Int])) {
      case (c, i) => c.insert(0, i)
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
  def insertStart(): DeltaBufferRDT[GList[Int]] = list.insert(0, -1)

  @Benchmark
  def insertEnd(): DeltaBufferRDT[GList[Int]] = list.insert(listSize, -1)
}
