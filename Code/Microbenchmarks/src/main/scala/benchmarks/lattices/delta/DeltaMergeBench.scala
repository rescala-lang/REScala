package benchmarks.lattices.delta

import kofre.base.{Bottom, DecomposeLattice}
import kofre.causality.{CausalContext, Dot}
import kofre.contextual.WithContext
import kofre.decompose.interfaces.RGAInterface.{RGA, RGASyntax}
import kofre.syntax.PermIdMutate.withID
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

  def makeCContext(replicaID: String): CausalContext = {
    val dots = (0L until size).map(Dot(replicaID, _)).toSet
    CausalContext.fromSet(dots)
  }

  @Setup
  def setup(): Unit = {
    val baseState: RGA[Long] = Bottom.empty[RGA[Long]]

    val deltaState = RGASyntax(baseState.named("")).insertAll(0, 0L to size).anon
    fullState = DecomposeLattice[RGA[Long]].merge(baseState, deltaState)

    plusOneDeltaState = fullState.insert(0, size)(withID(""))
    plusOneState = DecomposeLattice[RGA[Long]].merge(fullState, plusOneDeltaState)
  }

  @Benchmark
  def fullMerge: RGA[Long] = {
    DecomposeLattice[RGA[Long]].merge(fullState, plusOneState)
  }

  @Benchmark
  def fullDiff: Option[RGA[Long]] = {
    DecomposeLattice[RGA[Long]].diff(fullState, plusOneState)
  }

  @Benchmark
  def deltaMerge: RGA[Long] = {
    DecomposeLattice[RGA[Long]].diff(fullState, plusOneDeltaState) match {
      case Some(stateDiff) =>
        DecomposeLattice[RGA[Long]].merge(fullState, stateDiff)
      case None => fullState
    }
  }

  @Benchmark
  def deltaMergeNoDiff: RGA[Long] = {
    DecomposeLattice[RGA[Long]].merge(fullState, plusOneDeltaState)
  }
}
