package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations._
import kofre.decompose.DecomposeLattice
import kofre.decompose.interfaces.AWSetInterface.{AWSet, AWSetSyntax}
import kofre.syntax.AllPermissionsCtx

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

  type State = AWSet[String]

  var setAState: State        = _
  var setBState: State        = _
  var plusOneDelta: State     = _
  var setAStatePlusOne: State = _

  private def createSet(replicaID: String): State = {
    (0 until setSize).foldLeft(DecomposeLattice[State].empty) { (s, i) =>
      val delta = s.add(s"${i.toString}$replicaID")(AllPermissionsCtx.withID(replicaID))
      DecomposeLattice[State].merge(s, delta)
    }
  }

  @Setup
  def setup(): Unit = {
    setAState = createSet("a")
    setBState = createSet("b")

    plusOneDelta = setBState.add("hallo welt")(AllPermissionsCtx.withID("b"))
    setAStatePlusOne = DecomposeLattice[State].merge(setAState, setBState)
  }

  @Benchmark
  def create(): State = createSet("c")

  @Benchmark
  def addOne(): State = setAState.add("Hallo Welt")(AllPermissionsCtx.withID("a"))

  @Benchmark
  def merge(): State = DecomposeLattice[State].merge(setAState, setBState)

  @Benchmark
  def mergeSelf(): State = DecomposeLattice[State].merge(setAState, setBState)

  @Benchmark
  def mergeSelfPlusOne(): State = DecomposeLattice[State].merge(setAState, setAStatePlusOne)

  @Benchmark
  def mergeDelta(): State = DecomposeLattice[State].merge(setAState, plusOneDelta)
}
