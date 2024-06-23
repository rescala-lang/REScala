package benchmarks.lattices

import org.openjdk.jmh.annotations.*
import rdts.base.LocalUid.asId
import rdts.datatypes.alternatives.rga.Sequence
import rdts.datatypes.alternatives.rga.Sequence.{RGA, RGAOps}
import rdts.datatypes.contextual.CausalQueue
import rdts.dotted.Dotted

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class CausalQueueBench {

  @Param(Array("100", "1000", "10000", "100000"))
  var size: Int = scala.compiletime.uninitialized

  @Param(Array("10000"))
  var operations: Int = scala.compiletime.uninitialized

  var lca: Dotted[CausalQueue[Int]] = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    lca = (1 to size).foldLeft(Dotted(CausalQueue.empty[Int])) { (q, e) => q.mod(_.enqueue(using "lca".asId)(e)) }
  }

  def make(base: Dotted[CausalQueue[Int]], ops: Int, prefix: String) = {
    val s     = ops / 2
    val added = (1 to s).foldLeft(base) { (acc, v) => acc.mod(_.enqueue(using prefix.asId)(v)) }
    (1 to s).foldLeft(added) { (acc, _) => acc.mod(_.dequeue()) }
  }

  @Benchmark
  def create(): Dotted[CausalQueue[Int]] = make(lca, operations, "")

  @Benchmark
  def createAndMerge(): Dotted[CausalQueue[Int]] = {
    val left  = make(lca, operations, "left")
    val right = make(lca, operations, "right")

    val res = left merge right
    res
  }

}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class CausalQueueBenchWithRGA {

  @Param(Array("100", "1000", "10000", "100000"))
  var size: Int = scala.compiletime.uninitialized

  @Param(Array("10000"))
  var operations: Int = scala.compiletime.uninitialized

  var lca: RGA[Int] = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    lca = (1 to size).foldLeft(Sequence.empty[Int]) { (q, e) => q.prepend(e) }
  }

  def make(base: RGA[Int], ops: Int) = {
    val s     = ops / 2
    val added = (1 to s).foldLeft(base) { (acc, v) => acc.prepend(v) }
    (1 to s).foldLeft(added) { (acc, _) => acc.remove(Seq(acc.vertexIterator.next())) }
  }

  @Benchmark
  def create(): RGA[Int] = make(lca, operations)

  @Benchmark
  def createAndMerge(): RGA[Int] = {
    val left  = make(lca, operations)
    val right = make(lca, operations)

    val res = left merge right
    res
  }

}
