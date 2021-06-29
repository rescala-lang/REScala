package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.CContext.DietMapCContext
import rescala.extra.lattices.delta.crdt.reactive.AWSet

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

  var set: AWSet[Int, DietMapCContext] = _

  def createBySize(size: Int): AWSet[Int, DietMapCContext] = (0 until size).foldLeft(AWSet[Int, DietMapCContext]("a")) {
      case (s, e) => s.add(e)
    }

  @Setup
  def setup(): Unit = {
    set = createBySize(size)
  }

  @Benchmark
  def elements(): Set[Int] = set.elements

  @Benchmark
  def add(): AWSet[Int, DietMapCContext] = set.add(-1)

  @Benchmark
  def remove(): AWSet[Int, DietMapCContext] = set.remove(0)

  @Benchmark
  def removeBy(): AWSet[Int, DietMapCContext] = set.removeBy(_ == 0)

  @Benchmark
  def clear(): AWSet[Int, DietMapCContext] = set.clear()

  @Benchmark
  def construct(): AWSet[Int, DietMapCContext] = createBySize(size)
}
