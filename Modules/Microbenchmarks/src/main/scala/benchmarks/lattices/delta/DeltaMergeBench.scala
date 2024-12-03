package benchmarks.lattices.delta

import org.openjdk.jmh.annotations
import org.openjdk.jmh.annotations.*
import rdts.base.LocalUid.asId
import rdts.datatypes.contextual.ReplicatedList
import rdts.dotted.{Dotted, DottedLattice}
import rdts.time.{Dot, Dots}

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
  var size: Long = scala.compiletime.uninitialized

  var fullState: Dotted[ReplicatedList[Long]]         = scala.compiletime.uninitialized
  var plusOneState: Dotted[ReplicatedList[Long]]      = scala.compiletime.uninitialized
  var plusOneDeltaState: Dotted[ReplicatedList[Long]] = scala.compiletime.uninitialized

  def makeCContext(replicaID: String): Dots = {
    val dots = (0L until size).map(Dot(replicaID.asId.uid, _)).toSet
    Dots.from(dots)
  }

  @Setup
  def setup(): Unit = {
    val baseState: Dotted[ReplicatedList[Long]] = Dotted(ReplicatedList.empty)

    val deltaState: Dotted[ReplicatedList[Long]] =
      baseState.mod(_.insertAll(using "".asId)(0, 0L to size))
    fullState = DottedLattice[ReplicatedList[Long]].merge(baseState, deltaState)

    plusOneDeltaState = fullState.mod(_.insert(using "".asId)(0, size))
    plusOneState = DottedLattice[ReplicatedList[Long]].merge(fullState, plusOneDeltaState)
  }

  @Benchmark
  def fullMerge: Dotted[ReplicatedList[Long]] = {
    DottedLattice[ReplicatedList[Long]].merge(fullState, plusOneState)
  }

  @Benchmark
  def fullDiff: Option[Dotted[ReplicatedList[Long]]] = {
    DottedLattice.diff(fullState, plusOneState)
  }

  @Benchmark
  def deltaMerge: Dotted[ReplicatedList[Long]] = {
    DottedLattice.diff(fullState, plusOneDeltaState) match {
      case Some(stateDiff) =>
        DottedLattice[ReplicatedList[Long]].merge(fullState, stateDiff)
      case None => fullState
    }
  }

  @Benchmark
  def deltaMergeNoDiff: Dotted[ReplicatedList[Long]] = {
    DottedLattice[ReplicatedList[Long]].merge(fullState, plusOneDeltaState)
  }
}
