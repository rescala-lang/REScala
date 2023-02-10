package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import kofre.datatypes.GrowOnlyCounter
import kofre.base.Uid.asId

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class GCounterBench {

  @Param(Array("1", "10", "100", "1000"))
  var numReplicas: Int = _

  var counter: NamedDeltaBuffer[GrowOnlyCounter] = _

  @Setup
  def setup(): Unit = {
    counter = (1 until numReplicas).foldLeft(NamedDeltaBuffer("0", GrowOnlyCounter.zero).inc()(using "0".asId)) {
      case (c, n) =>
        val delta = NamedDeltaBuffer(n.toString, GrowOnlyCounter.zero).inc()(using n.toString.asId).deltaBuffer.head
        c.applyDelta(delta.replicaId, delta.anon)
    }
  }

  @Benchmark
  def value(): Int = counter.value

  @Benchmark
  def inc(): NamedDeltaBuffer[GrowOnlyCounter] = counter.inc()(using counter.replicaID)
}
