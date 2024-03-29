package benchmarks.lattices.delta.crdt

import rdts.base.Lattice
import rdts.dotted.Dotted
import org.openjdk.jmh.annotations._
import rdts.base.Uid.asId
import rdts.datatypes.contextual.ReplicatedSet

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class AWSetComparisonBench {

  @Param(Array("0", "1", "10", "100", "1000"))
  var setSize: Int = scala.compiletime.uninitialized

  type State = Dotted[ReplicatedSet[String]]

  var setAState: State        = scala.compiletime.uninitialized
  var setBState: State        = scala.compiletime.uninitialized
  var plusOneDelta: State     = scala.compiletime.uninitialized
  var setAStatePlusOne: State = scala.compiletime.uninitialized

  private def createSet(replicaID: String): State = {
    (0 until setSize).foldLeft(Dotted(ReplicatedSet.empty[String])) { (s, i) =>
      val delta = s.add(using replicaID.asId)(s"${i.toString}$replicaID")
      Lattice[State].merge(s, delta)
    }
  }

  @Setup
  def setup(): Unit = {
    setAState = createSet("a")
    setBState = createSet("b")

    plusOneDelta = setBState.add(using "b".asId)("hallo welt")
    setAStatePlusOne = Lattice[State].merge(setAState, setBState)
  }

  @Benchmark
  def create(): State = createSet("c")

  @Benchmark
  def addOne(): State = setAState.add(using "a".asId)("Hallo Welt")

  @Benchmark
  def merge(): State = Lattice[State].merge(setAState, setBState)

  @Benchmark
  def mergeSelf(): State = Lattice[State].merge(setAState, setBState)

  @Benchmark
  def mergeSelfPlusOne(): State = Lattice[State].merge(setAState, setAStatePlusOne)

  @Benchmark
  def mergeDelta(): State = Lattice[State].merge(setAState, plusOneDelta)
}
