package benchmarks.lattices.delta

import kofre.causality.{CausalContext, Dot}
import kofre.contextual.{ContextDecompose, ContextLattice, WithContext}
import kofre.predef.AddWinsSet
import org.openjdk.jmh.annotations
import org.openjdk.jmh.annotations._

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

  var fullState: WithContext[AddWinsSet[Long]]         = _
  var plusOneState: WithContext[AddWinsSet[Long]]      = _
  var plusOneDeltaState: WithContext[AddWinsSet[Long]] = _

  def makeCContext(replicaID: String): CausalContext = {
    val dots = (0L until size).map(Dot(replicaID, _)).toSet
    CausalContext.fromSet(dots)
  }

  @Setup
  def setup(): Unit = {
    val baseState = WithContext(AddWinsSet.empty[Long])

    val deltaState = baseState.named("").addAll(0L to size).anon
    fullState = ContextLattice[AddWinsSet[Long]].merge(baseState, deltaState)

    plusOneDeltaState = fullState.named("").add(size).anon
    plusOneState = ContextLattice[AddWinsSet[Long]].merge(fullState, plusOneDeltaState)
  }

  @Benchmark
  def fullMerge: WithContext[AddWinsSet[Long]] = {
    ContextLattice[AddWinsSet[Long]].merge(fullState, plusOneState)
  }

  @Benchmark
  def fullDiff: Option[WithContext[AddWinsSet[Long]]] = {
    ContextDecompose[AddWinsSet[Long]].diff(fullState, plusOneState)
  }

  @Benchmark
  def deltaMerge: WithContext[AddWinsSet[Long]] = {
    ContextDecompose[AddWinsSet[Long]].diff(fullState, plusOneDeltaState) match {
      case Some(stateDiff) =>
        ContextDecompose[AddWinsSet[Long]].merge(fullState, stateDiff)
      case None => fullState
    }
  }

  @Benchmark
  def deltaMergeNoDiff: WithContext[AddWinsSet[Long]] = {
    ContextDecompose[AddWinsSet[Long]].merge(fullState, plusOneDeltaState)
  }
}
