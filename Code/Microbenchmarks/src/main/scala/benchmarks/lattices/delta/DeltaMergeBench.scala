package benchmarks.lattices.delta

import kofre.time.{Dots, Dot}
import kofre.contextual.{ContextDecompose, WithContext}
import kofre.decompose.interfaces.RGA
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
class DeltaMergeBench {

  @Param(Array("1", "10", "100", "1000"))
  var size: Long = _

  var fullState: WithContext[RGA[Long]]         = _
  var plusOneState: WithContext[RGA[Long]]      = _
  var plusOneDeltaState: WithContext[RGA[Long]] = _

  def makeCContext(replicaID: String): Dots = {
    val dots = (0L until size).map(Dot(replicaID, _)).toSet
    Dots.fromSet(dots)
  }

  @Setup
  def setup(): Unit = {
    val baseState: WithContext[RGA[Long]] = WithContext(RGA.empty)

    val deltaState: WithContext[RGA[Long]] =
      baseState.named("").insertAll(0, 0L to size).anon
    fullState = ContextDecompose[RGA[Long]].merge(baseState, deltaState)

    plusOneDeltaState = fullState.named("").insert(0, size).anon
    plusOneState = ContextDecompose[RGA[Long]].merge(fullState, plusOneDeltaState)
  }

  @Benchmark
  def fullMerge: WithContext[RGA[Long]] = {
    ContextDecompose[RGA[Long]].merge(fullState, plusOneState)
  }

  @Benchmark
  def fullDiff: Option[WithContext[RGA[Long]]] = {
    ContextDecompose[RGA[Long]].diff(fullState, plusOneState)
  }

  @Benchmark
  def deltaMerge: WithContext[RGA[Long]] = {
    ContextDecompose[RGA[Long]].diff(fullState, plusOneDeltaState) match {
      case Some(stateDiff) =>
        ContextDecompose[RGA[Long]].merge(fullState, stateDiff)
      case None => fullState
    }
  }

  @Benchmark
  def deltaMergeNoDiff: WithContext[RGA[Long]] = {
    ContextDecompose[RGA[Long]].merge(fullState, plusOneDeltaState)
  }
}
