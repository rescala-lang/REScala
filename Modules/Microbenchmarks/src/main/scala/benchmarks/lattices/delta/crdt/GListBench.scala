package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import rdts.base.Uid.asId
import rdts.datatypes.GrowOnlyList

import java.util.concurrent.TimeUnit
import benchmarks.lattices.delta.crdt.NamedDeltaBuffer.mod


@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class GListBench {

  @Param(Array("0", "1", "10", "100", "1000"))
  var listSize: Int = scala.compiletime.uninitialized

  var list: NamedDeltaBuffer[GrowOnlyList[Int]] = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    list = (0 until listSize).foldLeft(NamedDeltaBuffer("a".asId, GrowOnlyList.empty[Int])) {
      case (c, i) => c.mod(_.insertGL(0, i))
    }
  }

  @Benchmark
  def toList: List[Int] = list.state.toList

  @Benchmark
  def size(): Int = list.state.size

  @Benchmark
  def readFirst(): Option[Int] = list.state.read(0)

  @Benchmark
  def readLast(): Option[Int] = list.state.read(listSize - 1)

  @Benchmark
  def insertStart(): NamedDeltaBuffer[GrowOnlyList[Int]] = list.mod(_.insertGL(0, -1))

  @Benchmark
  def insertEnd(): NamedDeltaBuffer[GrowOnlyList[Int]] = list.mod(_.insertGL(listSize, -1))
}
