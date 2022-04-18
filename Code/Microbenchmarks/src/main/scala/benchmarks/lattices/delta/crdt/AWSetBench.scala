package benchmarks.lattices.delta.crdt

import kofre.decompose.interfaces.AWSetInterface.{AWSet, AWSetSyntax}
import org.openjdk.jmh.annotations._
import rescala.extra.replication.containers.ReactiveDeltaCRDT

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

  var set: ReactiveDeltaCRDT[AWSet[Int]] = _

  def createBySize(size: Int): ReactiveDeltaCRDT[AWSet[Int]] =
    (0 until size).foldLeft(ReactiveDeltaCRDT[AWSet[Int]]("a")) {
      case (s, e) => s.add(e)
    }

  @Setup
  def setup(): Unit = {
    set = createBySize(size)
  }

  @Benchmark
  def elements(): Set[Int] = set.elements

  @Benchmark
  def add(): ReactiveDeltaCRDT[AWSet[Int]] = set.add(-1)

  @Benchmark
  def addAll(): ReactiveDeltaCRDT[AWSet[Int]] = ReactiveDeltaCRDT[AWSet[Int]]("a").addAll(0 until size)

  @Benchmark
  def remove(): ReactiveDeltaCRDT[AWSet[Int]] = set.remove(0)

  @Benchmark
  def removeBy(): ReactiveDeltaCRDT[AWSet[Int]] = set.removeBy((e: Int) => e == 0)

  @Benchmark
  def removeAll(): ReactiveDeltaCRDT[AWSet[Int]] = set.removeAll(set.elements)

  @Benchmark
  def clear(): ReactiveDeltaCRDT[AWSet[Int]] = set.clear()

  @Benchmark
  def construct(): ReactiveDeltaCRDT[AWSet[Int]] = createBySize(size)
}
