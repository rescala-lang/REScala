package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import rdts.base.LocalUid.asId
import rdts.datatypes.GrowOnlyCounter

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
  var numReplicas: Int = scala.compiletime.uninitialized

  var counter: NamedDeltaBuffer[GrowOnlyCounter] = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    counter = (1 until numReplicas).foldLeft(NamedDeltaBuffer("0".asId, GrowOnlyCounter.zero).map(_.inc())) {
      case (c, n) =>
        val delta = NamedDeltaBuffer(n.toString.asId, GrowOnlyCounter.zero).map(_.inc()).deltaBuffer.head
        c.applyDelta(delta.replicaId, delta.anon)
    }
  }

  @Benchmark
  def value(): Int = counter.state.value

  @Benchmark
  def inc(): NamedDeltaBuffer[GrowOnlyCounter] = counter.map(_.inc())
}
