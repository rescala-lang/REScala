package benchmarks.lattices.delta.crdt

import kofre.base.DecomposeLattice
import org.openjdk.jmh.annotations.*
import kofre.datatypes.PosNegCounter
import kofre.deprecated.containers.{DeltaBuffer, DeltaBufferRDT}
import kofre.dotted.{Dotted, DottedDecompose}
import kofre.syntax.Named

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

  var counter: DeltaBuffer[PosNegCounter] = _

  @Setup
  def setup(): Unit = {
    counter = (1 until numReplicas).foldLeft(DeltaBuffer("0", PosNegCounter.zero).inc()) {
      case (c, n) =>
        val delta = Named(kofre.base.Id.predefined(n.toString), PosNegCounter.zero).inc()
        c.applyDelta(delta.replicaId, delta.anon)
    }
  }

  @Benchmark
  def value(): Int = counter.value

  @Benchmark
  def inc(): DeltaBuffer[PosNegCounter] = counter.inc()

  @Benchmark
  def dec(): DeltaBuffer[PosNegCounter] = counter.dec()
}
