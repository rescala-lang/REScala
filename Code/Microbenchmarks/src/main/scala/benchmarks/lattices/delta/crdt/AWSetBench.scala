package benchmarks.lattices.delta.crdt

import kofre.causality.CausalContext
import org.openjdk.jmh.annotations._
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

  var set: AWSet[Int, CausalContext] = _

  def createBySize(size: Int): AWSet[Int, CausalContext] = (0 until size).foldLeft(AWSet[Int, CausalContext]("a")) {
    case (s, e) => s.add(e)
  }

  @Setup
  def setup(): Unit = {
    set = createBySize(size)
  }

  @Benchmark
  def elements(): Set[Int] = set.elements

  @Benchmark
  def add(): AWSet[Int, CausalContext] = set.add(-1)

  @Benchmark
  def addAll(): AWSet[Int, CausalContext] = AWSet[Int, CausalContext]("a").addAll(0 until size)

  @Benchmark
  def remove(): AWSet[Int, CausalContext] = set.remove(0)

  @Benchmark
  def removeBy(): AWSet[Int, CausalContext] = set.removeBy(_ == 0)

  @Benchmark
  def removeAll(): AWSet[Int, CausalContext] = set.removeAll(set.elements)

  @Benchmark
  def clear(): AWSet[Int, CausalContext] = set.clear()

  @Benchmark
  def construct(): AWSet[Int, CausalContext] = createBySize(size)
}
