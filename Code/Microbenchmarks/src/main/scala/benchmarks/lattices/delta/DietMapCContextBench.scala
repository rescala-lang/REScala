package benchmarks.lattices.delta

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.DietCC.DietMapCContext
import kofre.decompose.Dot

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class DietMapCContextBench {

  @Param(Array("1", "1000"))
  var size: Int = _

  var cca: DietMapCContext       = _
  var ccb: DietMapCContext       = _
  var cca1: DietMapCContext      = _
  var ccaSingle: DietMapCContext = _

  def makeCContext(replicaID: String): DietMapCContext = {
    val dots = (0 until size).map(Dot(replicaID, _)).toSet
    DietMapCContext.fromSet(dots)
  }

  @Setup
  def setup(): Unit = {
    cca = makeCContext("a")
    ccb = makeCContext("b")
    cca1 = DietMapCContext.union(cca, DietMapCContext.fromSet(Set(Dot("b", 5))))
    ccaSingle = DietMapCContext.fromSet(Set(Dot("a", size + 10)))
  }

  @Benchmark
  def union = DietMapCContext.union(cca, ccb)

  @Benchmark
  def unionSelf = DietMapCContext.union(cca, cca)

  @Benchmark
  def unionSelfPlusOne = DietMapCContext.union(cca, cca1)
}
