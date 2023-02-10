package benchmarks.lattices.delta.crdt

import kofre.base.DecomposeLattice
import org.openjdk.jmh.annotations.*
import kofre.datatypes.PosNegCounter
import kofre.dotted.{Dotted, DottedDecompose}
import kofre.syntax.Named
import kofre.base.Id.asId

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class PNCounterBench {

  @Param(Array("1", "10", "100", "1000"))
  var numReplicas: Int = _

  var counter: NamedDeltaBuffer[PosNegCounter] = _

  @Setup
  def setup(): Unit = {
    counter = (1 until numReplicas).foldLeft(NamedDeltaBuffer("0", PosNegCounter.zero).inc()(using "0".asId)) {
      case (c, n) =>
        given rid: kofre.syntax.ReplicaId = kofre.base.Id.predefined(n.toString)
        val delta = PosNegCounter.zero.inc()
        c.applyDelta(rid.replicaId, delta)
    }
  }

  @Benchmark
  def value(): Int = counter.value

  @Benchmark
  def inc(): NamedDeltaBuffer[PosNegCounter] = counter.inc()(using counter.replicaID)

  @Benchmark
  def dec(): NamedDeltaBuffer[PosNegCounter] = counter.dec()(using counter.replicaID)
}
