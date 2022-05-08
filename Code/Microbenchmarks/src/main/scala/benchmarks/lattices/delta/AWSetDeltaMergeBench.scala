package benchmarks.lattices.delta

import kofre.base.DecomposeLattice
import org.openjdk.jmh.annotations
import org.openjdk.jmh.annotations._
import kofre.causality.{CausalContext, Dot}
import kofre.predef.AddWinsSet
import kofre.syntax.AllPermissionsCtx

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

  var fullState: AddWinsSet[Long]         = _
  var plusOneState: AddWinsSet[Long]      = _
  var plusOneDeltaState: AddWinsSet[Long] = _

  def makeCContext(replicaID: String): CausalContext = {
    val dots = (0L until size).map(Dot(replicaID, _)).toSet
    CausalContext.fromSet(dots)
  }

  @Setup
  def setup(): Unit = {
    val baseState = DecomposeLattice[AddWinsSet[Long]].empty

    val deltaState = baseState.addAll(0L to size)(AllPermissionsCtx.withID(""))
    fullState = DecomposeLattice[AddWinsSet[Long]].merge(baseState, deltaState)

    plusOneDeltaState = fullState.add(size)(AllPermissionsCtx.withID(""))
    plusOneState = DecomposeLattice[AddWinsSet[Long]].merge(fullState, plusOneDeltaState)
  }

  @Benchmark
  def fullMerge: AddWinsSet[Long] = {
    DecomposeLattice[AddWinsSet[Long]].merge(fullState, plusOneState)
  }

  @Benchmark
  def fullDiff: Option[AddWinsSet[Long]] = {
    DecomposeLattice[AddWinsSet[Long]].diff(fullState, plusOneState)
  }

  @Benchmark
  def deltaMerge: AddWinsSet[Long] = {
    DecomposeLattice[AddWinsSet[Long]].diff(fullState, plusOneDeltaState) match {
      case Some(stateDiff) =>
        DecomposeLattice[AddWinsSet[Long]].merge(fullState, stateDiff)
      case None => fullState
    }
  }

  @Benchmark
  def deltaMergeNoDiff: AddWinsSet[Long] = {
    DecomposeLattice[AddWinsSet[Long]].merge(fullState, plusOneDeltaState)
  }
}
