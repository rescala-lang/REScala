package benchmarks.lattices.delta

import org.openjdk.jmh.annotations._
import kofre.causality.{CContext, CausalContext, Dot}

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class DietMapCContextBench {

  @Param(Array("1", "1000"))
  var size: Long = _

  var cca: CausalContext       = _
  var ccb: CausalContext       = _
  var cca1: CausalContext      = _
  var ccaSingle: CausalContext = _

  private def makeCContext(replicaID: String, mul: Long, off: Long, len: Long): CausalContext = {
    val ranges = Range.Long(0L, size, 1).map(i => Range.Long(i * mul + off, i * mul + len + off, 1))
    val dots   = ranges.flatten.map(Dot(replicaID, _)).toSet
    CausalContext.fromSet(dots)
  }

  @Setup
  def setup(): Unit = {
    cca = makeCContext("a", 10, 0, 7)
    ccb = makeCContext("b", 10, 5, 7)
    cca1 = CContext.intTreeCC.union(cca, CausalContext.fromSet(Set(Dot("b", 5))))
    ccaSingle = CausalContext.fromSet(Set(Dot("a", size + 10)))
  }

  @Benchmark
  def merge = CContext.intTreeCC.union(cca, ccb)

  @Benchmark
  def mergeSelf = CContext.intTreeCC.union(cca, cca)

  @Benchmark
  def mergeSelfPlusOne = CContext.intTreeCC.union(cca, cca1)
}
