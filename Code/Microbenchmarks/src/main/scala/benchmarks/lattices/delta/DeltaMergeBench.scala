package benchmarks.lattices.delta

import org.openjdk.jmh.annotations
import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.crdt.reactive._
import kofre.decompose.interfaces.RGAInterface
import kofre.decompose.UIJDLattice
import kofre.causality.{CausalContext, Dot}

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

  var fullState: ListRDT.State[Long]         = _
  var plusOneState: ListRDT.State[Long]      = _
  var plusOneDeltaState: ListRDT.State[Long] = _

  def makeCContext(replicaID: String): CausalContext = {
    val dots = (0L until size).map(Dot(replicaID, _)).toSet
    CausalContext.fromSet(dots)
  }

  @Setup
  def setup(): Unit = {
    val baseState = UIJDLattice[ListRDT.State[Long]].bottom

    val deltaState = RGAInterface.insertAll[Long](0, 0L to size).apply("", baseState)
    fullState = UIJDLattice[ListRDT.State[Long]].merge(baseState, deltaState)

    plusOneDeltaState = RGAInterface.insert[Long](0, size).apply("", fullState)
    plusOneState = UIJDLattice[ListRDT.State[Long]].merge(fullState, plusOneDeltaState)
  }

  @Benchmark
  def fullMerge: ListRDT.State[Long] = {
    UIJDLattice[ListRDT.State[Long]].merge(fullState, plusOneState)
  }

  @Benchmark
  def fullDiff: Option[ListRDT.State[Long]] = {
    UIJDLattice[ListRDT.State[Long]].diff(fullState, plusOneState)
  }

  @Benchmark
  def deltaMerge: ListRDT.State[Long] = {
    UIJDLattice[ListRDT.State[Long]].diff(fullState, plusOneDeltaState) match {
      case Some(stateDiff) =>
        UIJDLattice[ListRDT.State[Long]].merge(fullState, stateDiff)
      case None => fullState
    }
  }

  @Benchmark
  def deltaMergeNoDiff: ListRDT.State[Long] = {
    UIJDLattice[ListRDT.State[Long]].merge(fullState, plusOneDeltaState)
  }
}
