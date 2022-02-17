package benchmarks.lattices.delta.crdt

import kofre.causality.CausalContext
import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.crdt.reactive.RCounter

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

  var counter: RCounter[CausalContext] = _

  @Setup
  def setup(): Unit = {
    counter = (1 until numReplicas).foldLeft(RCounter[CausalContext]("0").increment()) {
      case (c, n) =>
        val delta = RCounter(n.toString).increment().deltaBuffer.head
        c.applyDelta(delta)
    }
  }

  @Benchmark
  def value(): Int = counter.value

  @Benchmark
  def fresh(): RCounter[CausalContext] = counter.fresh()

  @Benchmark
  def increment(): RCounter[CausalContext] = counter.increment()

  @Benchmark
  def decrement(): RCounter[CausalContext] = counter.decrement()

  @Benchmark
  def reset(): RCounter[CausalContext] = counter.reset()
}
