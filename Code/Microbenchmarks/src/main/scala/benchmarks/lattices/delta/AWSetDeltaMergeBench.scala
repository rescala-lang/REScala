package benchmarks.lattices.delta

import org.openjdk.jmh.annotations
import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.DietCC._
import rescala.extra.lattices.delta.crdt.reactive._
import kofre.decompose.interfaces.AWSetInterface
import kofre.decompose.{Dot, UIJDLattice}

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
  var size: Int = _

  var fullState: AWSet.State[Int, DietMapCContext]         = _
  var plusOneState: AWSet.State[Int, DietMapCContext]      = _
  var plusOneDeltaState: AWSet.State[Int, DietMapCContext] = _

  def makeCContext(replicaID: String): DietMapCContext = {
    val dots = (0 until size).map(Dot(replicaID, _)).toSet
    DietMapCContext.fromSet(dots)
  }

  @Setup
  def setup(): Unit = {
    val baseState = UIJDLattice[AWSet.State[Int, DietMapCContext]].bottom

    val deltaState = AWSetInterface.addAll[Int, DietMapCContext](0 to size).apply("", baseState)
    fullState = UIJDLattice[AWSet.State[Int, DietMapCContext]].merge(baseState, deltaState)

    plusOneDeltaState = AWSetInterface.add[Int, DietMapCContext](size).apply("", fullState)
    plusOneState = UIJDLattice[AWSet.State[Int, DietMapCContext]].merge(fullState, plusOneDeltaState)
  }

  @Benchmark
  def fullMerge: AWSet.State[Int, DietMapCContext] = {
    UIJDLattice[AWSet.State[Int, DietMapCContext]].merge(fullState, plusOneState)
  }

  @Benchmark
  def fullDiff: Option[AWSet.State[Int, DietMapCContext]] = {
    UIJDLattice[AWSet.State[Int, DietMapCContext]].diff(fullState, plusOneState)
  }

  @Benchmark
  def deltaMerge: AWSet.State[Int, DietMapCContext] = {
    UIJDLattice[AWSet.State[Int, DietMapCContext]].diff(fullState, plusOneDeltaState) match {
      case Some(stateDiff) =>
        UIJDLattice[AWSet.State[Int, DietMapCContext]].merge(fullState, stateDiff)
      case None => fullState
    }
  }

  @Benchmark
  def deltaMergeNoDiff: AWSet.State[Int, DietMapCContext] = {
    UIJDLattice[AWSet.State[Int, DietMapCContext]].merge(fullState, plusOneDeltaState)
  }
}
