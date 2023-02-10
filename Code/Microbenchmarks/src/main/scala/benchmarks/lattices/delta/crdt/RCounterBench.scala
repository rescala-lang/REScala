package benchmarks.lattices.delta.crdt

import kofre.datatypes.alternatives.ResettableCounter
import org.openjdk.jmh.annotations.*
import kofre.dotted.Dotted

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class RCounterBench {

  @Param(Array("1", "10", "100", "1000"))
  var numReplicas: Int = _

  var counter: DeltaBufferDotted[ResettableCounter] = _

  @Setup
  def setup(): Unit = {
    counter = (1 until numReplicas).foldLeft(NamedDeltaBuffer.dotted("0", ResettableCounter.zero).increment()) {
      case (c, n) =>
        val delta = Dotted(ResettableCounter.zero).named(n.toString).increment()
        c.applyDelta(delta.replicaId, delta.anon)
    }
  }

  @Benchmark
  def value(): Int = counter.value

  @Benchmark
  def fresh(): DeltaBufferDotted[ResettableCounter] = counter.fresh()

  @Benchmark
  def increment(): DeltaBufferDotted[ResettableCounter] = counter.increment()

  @Benchmark
  def decrement(): DeltaBufferDotted[ResettableCounter] = counter.decrement()

  @Benchmark
  def reset(): DeltaBufferDotted[ResettableCounter] = counter.reset()
}
