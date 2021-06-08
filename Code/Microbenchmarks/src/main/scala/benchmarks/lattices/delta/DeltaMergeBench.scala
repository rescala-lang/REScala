package benchmarks.lattices.delta

import org.openjdk.jmh.annotations
import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.CContext._
import rescala.extra.lattices.delta.crdt.reactive._
import rescala.extra.lattices.delta.interfaces.RGAInterface
import rescala.extra.lattices.delta.{Dot, UIJDLattice}

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@annotations.State(Scope.Thread)
class DeltaMergeBench {

//  @Param(Array("1", "10", "100", "1000"))
  @Param(Array("1000"))
  var size: Int = _

  var fullState: RGA.State[Int, DietMapCContext]         = _
  var plusOneState: RGA.State[Int, DietMapCContext]      = _
  var plusOneDeltaState: RGA.State[Int, DietMapCContext] = _

  def makeCContext(replicaID: String): DietMapCContext = {
    val dots = (0 until size).map(Dot(replicaID, _)).toSet
    DietMapCContext.fromSet(dots)
  }

  @Setup
  def setup(): Unit = {
    val baseState = UIJDLattice[RGA.State[Int, DietMapCContext]].bottom

    val deltaState = RGAInterface.insertAll[Int, DietMapCContext](0, 0 to size).apply("", baseState)
    fullState = UIJDLattice[RGA.State[Int, DietMapCContext]].merge(baseState, deltaState)

    plusOneDeltaState = RGAInterface.insert[Int, DietMapCContext](0, size).apply("", fullState)
    plusOneState = UIJDLattice[RGA.State[Int, DietMapCContext]].merge(fullState, plusOneDeltaState)
  }

  @Benchmark
  def fullMerge: RGA.State[Int, DietMapCContext] = {
    UIJDLattice[RGA.State[Int, DietMapCContext]].merge(fullState, plusOneState)
  }

  @Benchmark
  def fullDiff: Option[RGA.State[Int, DietMapCContext]] = {
    UIJDLattice[RGA.State[Int, DietMapCContext]].diff(fullState, plusOneState)
  }

  @Benchmark
  def deltaMerge: RGA.State[Int, DietMapCContext] = {
    UIJDLattice[RGA.State[Int, DietMapCContext]].diff(fullState, plusOneDeltaState) match {
      case Some(stateDiff) =>
        UIJDLattice[RGA.State[Int, DietMapCContext]].merge(fullState, stateDiff)
      case None => fullState
    }
  }

  @Benchmark
  def deltaMergeNoDiff: RGA.State[Int, DietMapCContext] = {
    UIJDLattice[RGA.State[Int, DietMapCContext]].merge(fullState, plusOneDeltaState)
  }
}
