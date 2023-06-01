package benchmarks.lattices.delta.crdt

import kofre.base.Lattice
import kofre.dotted.Dotted
import org.openjdk.jmh.annotations._
import kofre.base.Uid.asId
import kofre.datatypes.contextual.AddWinsSet

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
  var setSize: Int = _

  type State = Dotted[AddWinsSet[String]]

  var setAState: State        = _
  var setBState: State        = _
  var plusOneDelta: State     = _
  var setAStatePlusOne: State = _

  private def createSet(replicaID: String): State = {
    (0 until setSize).foldLeft(Dotted(AddWinsSet.empty[String])) { (s, i) =>
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
