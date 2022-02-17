package benchmarks.lattices.delta

import org.openjdk.jmh.annotations
import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.DietCC._
import rescala.extra.lattices.delta.crdt.reactive._
import kofre.decompose.interfaces.AWSetInterface
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
class AWSetDeltaMergeBench {

  @Param(Array("1", "10", "100", "1000"))
  var size: Long = _

  var fullState: AWSet.State[Long, DietMapCContext]         = _
  var plusOneState: AWSet.State[Long, DietMapCContext]      = _
  var plusOneDeltaState: AWSet.State[Long, DietMapCContext] = _

  def makeCContext(replicaID: String): DietMapCContext = {
    val dots = (0L until size).map(Dot(replicaID, _)).toSet
    DietMapCContext.fromSet(dots)
  }

  @Setup
  def setup(): Unit = {
    val baseState = UIJDLattice[AWSet.State[Long, DietMapCContext]].bottom

    val deltaState = AWSetInterface.addAll[Long, DietMapCContext](0L to size).apply("", baseState)
    fullState = UIJDLattice[AWSet.State[Long, DietMapCContext]].merge(baseState, deltaState)

    plusOneDeltaState = AWSetInterface.add[Long, DietMapCContext](size).apply("", fullState)
    plusOneState = UIJDLattice[AWSet.State[Long, DietMapCContext]].merge(fullState, plusOneDeltaState)
  }

  @Benchmark
  def fullMerge: AWSet.State[Long, DietMapCContext] = {
    UIJDLattice[AWSet.State[Long, DietMapCContext]].merge(fullState, plusOneState)
  }

  @Benchmark
  def fullDiff: Option[AWSet.State[Long, DietMapCContext]] = {
    UIJDLattice[AWSet.State[Long, DietMapCContext]].diff(fullState, plusOneState)
  }

  @Benchmark
  def deltaMerge: AWSet.State[Long, DietMapCContext] = {
    UIJDLattice[AWSet.State[Long, DietMapCContext]].diff(fullState, plusOneDeltaState) match {
      case Some(stateDiff) =>
        UIJDLattice[AWSet.State[Long, DietMapCContext]].merge(fullState, stateDiff)
      case None => fullState
    }
  }

  @Benchmark
  def deltaMergeNoDiff: AWSet.State[Long, DietMapCContext] = {
    UIJDLattice[AWSet.State[Long, DietMapCContext]].merge(fullState, plusOneDeltaState)
  }
}
