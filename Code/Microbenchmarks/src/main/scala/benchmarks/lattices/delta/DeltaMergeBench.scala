package benchmarks.lattices.delta

import org.openjdk.jmh.annotations
import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.DietCC._
import rescala.extra.lattices.delta.crdt.reactive._
import kofre.decompose.interfaces.RGAInterface
import kofre.decompose.{UIJDLattice}
import kofre.causality.Dot

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@annotations.State(Scope.Thread)
class DeltaMergeBench {

  @Param(Array("1", "10", "100", "1000"))
  var size: Long = _

  var fullState: RGA.State[Long, DietMapCContext]         = _
  var plusOneState: RGA.State[Long, DietMapCContext]      = _
  var plusOneDeltaState: RGA.State[Long, DietMapCContext] = _

  def makeCContext(replicaID: String): DietMapCContext = {
    val dots = (0L until size).map(Dot(replicaID, _)).toSet
    DietMapCContext.fromSet(dots)
  }

  @Setup
  def setup(): Unit = {
    val baseState = UIJDLattice[RGA.State[Long, DietMapCContext]].bottom

    val deltaState = RGAInterface.insertAll[Long, DietMapCContext](0, 0L to size).apply("", baseState)
    fullState = UIJDLattice[RGA.State[Long, DietMapCContext]].merge(baseState, deltaState)

    plusOneDeltaState = RGAInterface.insert[Long, DietMapCContext](0, size).apply("", fullState)
    plusOneState = UIJDLattice[RGA.State[Long, DietMapCContext]].merge(fullState, plusOneDeltaState)
  }

  @Benchmark
  def fullMerge: RGA.State[Long, DietMapCContext] = {
    UIJDLattice[RGA.State[Long, DietMapCContext]].merge(fullState, plusOneState)
  }

  @Benchmark
  def fullDiff: Option[RGA.State[Long, DietMapCContext]] = {
    UIJDLattice[RGA.State[Long, DietMapCContext]].diff(fullState, plusOneState)
  }

  @Benchmark
  def deltaMerge: RGA.State[Long, DietMapCContext] = {
    UIJDLattice[RGA.State[Long, DietMapCContext]].diff(fullState, plusOneDeltaState) match {
      case Some(stateDiff) =>
        UIJDLattice[RGA.State[Long, DietMapCContext]].merge(fullState, stateDiff)
      case None => fullState
    }
  }

  @Benchmark
  def deltaMergeNoDiff: RGA.State[Long, DietMapCContext] = {
    UIJDLattice[RGA.State[Long, DietMapCContext]].merge(fullState, plusOneDeltaState)
  }
}
