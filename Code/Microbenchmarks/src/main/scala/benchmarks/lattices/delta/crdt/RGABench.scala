package benchmarks.lattices.delta.crdt

import kofre.datatypes.ReplicatedList
import kofre.syntax.{DeltaBuffer, DeltaBufferDotted}
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class RGABench {

  @Param(Array("0", "1", "10", "100", "1000"))
  var rgaSize: Int = _

  type SUT = DeltaBufferDotted[ReplicatedList[Int]]

  var rga: SUT        = _
  var rgaCleared: SUT = _

  @Setup
  def setup(): Unit = {
    rga = DeltaBuffer.dotted("a", ReplicatedList.empty[Int]).appendAll(0 until rgaSize)
    rgaCleared = rga.clear()
  }

  @Benchmark
  def readFirst(): Option[Int] = rga.read(0)

  @Benchmark
  def readLast(): Option[Int] = rga.read(rgaSize - 1)

  @Benchmark
  def size(): Int = rga.size

  @Benchmark
  def toList: List[Int] = rga.toList

  @Benchmark
  def prepend(): SUT = rga.prepend(-1)

  @Benchmark
  def append(): SUT = rga.append(rgaSize)

  @Benchmark
  def prependTen(): SUT = rga.prependAll(-10 to -1)

  @Benchmark
  def appendTen(): SUT = rga.appendAll(rgaSize until rgaSize + 10)

  @Benchmark
  def updateFirst(): SUT = rga.update(0, -1)

  @Benchmark
  def updateLast(): SUT = rga.update(rgaSize - 1, -1)

  @Benchmark
  def deleteFirst(): SUT = rga.delete(0)

  @Benchmark
  def deleteLast(): SUT = rga.delete(rgaSize - 1)

  @Benchmark
  def clear(): SUT = rga.clear()

  @Benchmark
  def purgeTombstones(): SUT = rgaCleared.purgeTombstones()
}
