package benchmarks.lattices.delta.crdt

import kofre.causality.{CContext, CausalContext}
import org.openjdk.jmh.annotations._
import kofre.decompose.UIJDLattice
import rescala.extra.lattices.delta.crdt.basic.AWSet
import kofre.decompose.interfaces.AWSetInterface

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

  type State = AWSet.State[String, CausalContext]

  var setAState: State        = _
  var setBState: State        = _
  var plusOneDelta: State     = _
  var setAStatePlusOne: State = _

  private def createSet(replicaID: String): State = {
    (0 until setSize).foldLeft(UIJDLattice[State].bottom) { (s, i) =>
      val delta = AWSetInterface.add(i.toString + replicaID)(CContext.intTreeCC) .apply(replicaID, s)
      UIJDLattice[State].merge(s, delta)
    }
  }

  @Setup
  def setup(): Unit = {
    setAState = createSet("a")
    setBState = createSet("b")

    plusOneDelta = AWSetInterface.add("hallo welt")(CContext.intTreeCC).apply("b", setBState)
    setAStatePlusOne = UIJDLattice[State].merge(setAState, setBState)
  }

  @Benchmark
  def create(): State = createSet("c")

  @Benchmark
  def addOne(): State = AWSetInterface.add("Hallo Welt")(CContext.intTreeCC).apply("a", setAState)

  @Benchmark
  def merge(): State = UIJDLattice[State].merge(setAState, setBState)

  @Benchmark
  def mergeSelf(): State = UIJDLattice[State].merge(setAState, setBState)

  @Benchmark
  def mergeSelfPlusOne(): State = UIJDLattice[State].merge(setAState, setAStatePlusOne)

  @Benchmark
  def mergeDelta(): State = UIJDLattice[State].merge(setAState, plusOneDelta)
}
