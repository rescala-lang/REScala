package benchmarks.lattices

import rdts.time.ArrayRanges
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class ArrayContextBench {

  @Param(Array("1", "1000"))
  var size: Int = scala.compiletime.uninitialized

  var rep1Set: ArrayRanges        = scala.compiletime.uninitialized
  var rep1SetPlusOne: ArrayRanges = scala.compiletime.uninitialized
  var rep2Set: ArrayRanges        = scala.compiletime.uninitialized
  var rep1single: ArrayRanges     = scala.compiletime.uninitialized

  private def makeRep(mul: Int, off: Int, len: Int): ArrayRanges = {
    val ranges = Range(0, size).map(i => Range(i * mul + off, i * mul + len + off))
    ArrayRanges(ranges.map(r => (r.start.toLong, r.end.toLong)))
  }

  @Setup
  def setup(): Unit = {
    rep1Set = makeRep(10, 0, 7)
    rep2Set = makeRep(10, 5, 7)
    rep1SetPlusOne = rep1Set.add(5)
  }

  @Benchmark
  def merge() = rep1Set.union(rep2Set)

  @Benchmark
  def mergeSelf() = (rep1Set union rep1Set)

  @Benchmark
  def mergeSelfPlusOne() = (rep1Set union rep1SetPlusOne)

}
