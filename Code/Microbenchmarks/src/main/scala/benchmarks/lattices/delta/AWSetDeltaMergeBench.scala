package benchmarks.lattices.delta

import kofre.base.DecomposeLattice
import org.openjdk.jmh.annotations
import org.openjdk.jmh.annotations._
import kofre.causality.{CausalContext, Dot}
import kofre.predef.AddWinsSet.{AWSet, AWSetSyntax}
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

  var fullState: AWSet[Long]         = _
  var plusOneState: AWSet[Long]      = _
  var plusOneDeltaState: AWSet[Long] = _

  def makeCContext(replicaID: String): CausalContext = {
    val dots = (0L until size).map(Dot(replicaID, _)).toSet
    CausalContext.fromSet(dots)
  }

  @Setup
  def setup(): Unit = {
    val baseState = DecomposeLattice[AWSet[Long]].empty

    val deltaState = baseState.addAll(0L to size)(AllPermissionsCtx.withID(""))
    fullState = DecomposeLattice[AWSet[Long]].merge(baseState, deltaState)

    plusOneDeltaState = fullState.add(size)(AllPermissionsCtx.withID(""))
    plusOneState = DecomposeLattice[AWSet[Long]].merge(fullState, plusOneDeltaState)
  }

  @Benchmark
  def fullMerge: AWSet[Long] = {
    DecomposeLattice[AWSet[Long]].merge(fullState, plusOneState)
  }

  @Benchmark
  def fullDiff: Option[AWSet[Long]] = {
    DecomposeLattice[AWSet[Long]].diff(fullState, plusOneState)
  }

  @Benchmark
  def deltaMerge: AWSet[Long] = {
    DecomposeLattice[AWSet[Long]].diff(fullState, plusOneDeltaState) match {
      case Some(stateDiff) =>
        DecomposeLattice[AWSet[Long]].merge(fullState, stateDiff)
      case None => fullState
    }
  }

  @Benchmark
  def deltaMergeNoDiff: AWSet[Long] = {
    DecomposeLattice[AWSet[Long]].merge(fullState, plusOneDeltaState)
  }
}
