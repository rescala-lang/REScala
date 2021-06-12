package benchmarks.lattices.delta.crdt.basic

import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.crdt.reactive.LexCounter

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class LexCounterBench {

  @Param(Array("1", "10", "100", "1000"))
  var numReplicas: Int = _

  var counter: LexCounter = _

  @Setup
  def setup(): Unit = {
    counter = (1 until numReplicas).foldLeft(LexCounter("0").inc()) {
      case (c, n) =>
        val delta = LexCounter(n.toString).inc().deltaBuffer.head
        c.applyDelta(delta)
    }
  }

  @Benchmark
  def value(): Int = counter.value

  @Benchmark
  def inc(): LexCounter = counter.inc()

  @Benchmark
  def dec(): LexCounter = counter.dec()
}
