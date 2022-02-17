package benchmarks.lattices

import kofre.causality.IntTreeContext
import kofre.causality.impl.IntTree

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import kofre.{IdUtil, Lattice}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class ContextBench {

  @Param(Array("1", "1000"))
  var size: Int = _

  var rep1Set: IntTreeContext        = _
  var rep1SetPlusOne: IntTreeContext = _
  var rep2Set: IntTreeContext        = _
  val rep1id                  = IdUtil.genId()
  val rep2id                  = IdUtil.genId()
  var rep1single: IntTreeContext = _

  private def makeRep(rep: IdUtil.Id, mul: Int, off: Int, len: Int): IntTreeContext = {
    val ranges = Range(0, size).map(i => Range(i * mul + off, i * mul + len + off))
    IntTreeContext(Map(rep -> IntTree.fromIterator(ranges.flatten.iterator)))
  }

  @Setup
  def setup(): Unit = {
    rep1Set = makeRep(rep1id, 10, 0, 7)
    rep2Set = makeRep(rep2id, 10, 5, 7)
    rep1SetPlusOne = rep1Set.add(rep2id, 5)
    rep1single = IntTreeContext.empty.add(rep1id, size + 10)
  }

  @Benchmark
  def merge() = Lattice.merge(rep1Set, rep2Set)

  @Benchmark
  def mergeSelf() = Lattice.merge(rep1Set, rep1Set)

  @Benchmark
  def mergeSelfPlusOne() = Lattice.merge(rep1Set, rep1SetPlusOne)

  @Benchmark
  def diffSelf() = rep1Set.diff(rep1Set)

  @Benchmark
  def diffOther() = rep1Set.diff(rep2Set)

  @Benchmark
  def diffSingle() = rep1SetPlusOne.diff(rep1Set)

  @Benchmark
  def intersectSelf() = rep1Set.intersect(rep1Set)

  @Benchmark
  def intersectOther() = rep1Set.intersect(rep2Set)

}
