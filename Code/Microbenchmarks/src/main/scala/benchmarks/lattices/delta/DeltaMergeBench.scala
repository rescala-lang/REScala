package benchmarks.lattices.delta

import kofre.time.{Dots, Dot}
import kofre.decompose.interfaces.RGA
import kofre.dotted.{ContextDecompose, Dotted}
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

  var fullState: Dotted[RGA[Long]]         = _
  var plusOneState: Dotted[RGA[Long]]      = _
  var plusOneDeltaState: Dotted[RGA[Long]] = _

  def makeCContext(replicaID: String): Dots = {
    val dots = (0L until size).map(Dot(replicaID, _)).toSet
    Dots.fromSet(dots)
  }

  @Setup
  def setup(): Unit = {
    val baseState: Dotted[RGA[Long]] = Dotted(RGA.empty)

    val deltaState: Dotted[RGA[Long]] =
      baseState.named("").insertAll(0, 0L to size).anon
    fullState = ContextDecompose[RGA[Long]].merge(baseState, deltaState)

    plusOneDeltaState = fullState.named("").insert(0, size).anon
    plusOneState = ContextDecompose[RGA[Long]].merge(fullState, plusOneDeltaState)
  }

  @Benchmark
  def fullMerge: Dotted[RGA[Long]] = {
    ContextDecompose[RGA[Long]].merge(fullState, plusOneState)
  }

  @Benchmark
  def fullDiff: Option[Dotted[RGA[Long]]] = {
    ContextDecompose[RGA[Long]].diff(fullState, plusOneState)
  }

  @Benchmark
  def deltaMerge: Dotted[RGA[Long]] = {
    ContextDecompose[RGA[Long]].diff(fullState, plusOneDeltaState) match {
      case Some(stateDiff) =>
        ContextDecompose[RGA[Long]].merge(fullState, stateDiff)
      case None => fullState
    }
  }

  @Benchmark
  def deltaMergeNoDiff: Dotted[RGA[Long]] = {
    ContextDecompose[RGA[Long]].merge(fullState, plusOneDeltaState)
  }
}
