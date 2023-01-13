package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations._
import kofre.datatypes.GrowOnlyCounter
import kofre.deprecated.containers.DeltaBufferDotted

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

  var counter: DeltaBufferDotted[GrowOnlyCounter] = _

  @Setup
  def setup(): Unit = {
    counter = (1 until numReplicas).foldLeft(DeltaBufferDotted.empty("0", GrowOnlyCounter.zero).inc()) {
      case (c, n) =>
        val delta = DeltaBufferDotted(n.toString, GrowOnlyCounter.zero).inc().deltaBuffer.head
        c.applyDelta(delta.replicaId, delta.anon)
    }
  }

  @Benchmark
  def value(): Int = counter.value

  @Benchmark
  def inc(): DeltaBufferDotted[GrowOnlyCounter] = counter.inc()
}
