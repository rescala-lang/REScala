package benchmarks.lattices.delta

import kofre.time.{Dots, Dot}
import kofre.datatypes.AddWinsSet
import kofre.dotted.{DottedDecompose, DottedLattice, Dotted}
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

  var fullState: Dotted[AddWinsSet[Long]]         = _
  var plusOneState: Dotted[AddWinsSet[Long]]      = _
  var plusOneDeltaState: Dotted[AddWinsSet[Long]] = _

  def makeCContext(replicaID: String): Dots = {
    val dots = (0L until size).map(Dot(replicaID, _)).toSet
    Dots.fromSet(dots)
  }

  @Setup
  def setup(): Unit = {
    val baseState = Dotted(AddWinsSet.empty[Long])

    val deltaState = baseState.named("").addAll(0L to size).anon
    fullState = DottedLattice[AddWinsSet[Long]].merge(baseState, deltaState)

    plusOneDeltaState = fullState.named("").add(size).anon
    plusOneState = DottedLattice[AddWinsSet[Long]].merge(fullState, plusOneDeltaState)
  }

  @Benchmark
  def fullMerge: Dotted[AddWinsSet[Long]] = {
    DottedLattice[AddWinsSet[Long]].merge(fullState, plusOneState)
  }

  @Benchmark
  def fullDiff: Option[Dotted[AddWinsSet[Long]]] = {
    DottedDecompose[AddWinsSet[Long]].diff(fullState, plusOneState)
  }

  @Benchmark
  def deltaMerge: Dotted[AddWinsSet[Long]] = {
    DottedDecompose[AddWinsSet[Long]].diff(fullState, plusOneDeltaState) match {
      case Some(stateDiff) =>
        DottedDecompose[AddWinsSet[Long]].merge(fullState, stateDiff)
      case None => fullState
    }
  }

  @Benchmark
  def deltaMergeNoDiff: Dotted[AddWinsSet[Long]] = {
    DottedDecompose[AddWinsSet[Long]].merge(fullState, plusOneDeltaState)
  }
}
