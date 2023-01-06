package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations._
import kofre.decompose.interfaces.RCounterInterface.RCounter
import kofre.decompose.interfaces.RCounterInterface.RCounterSyntax
import kofre.decompose.interfaces.RCounterInterface
import kofre.deprecated.containers.DeltaBufferRDT

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

  var counter: DeltaBufferRDT[RCounter] = _

  @Setup
  def setup(): Unit = {
    counter = (1 until numReplicas).foldLeft(DeltaBufferRDT("0", RCounterInterface.zero).increment()) {
      case (c, n) =>
        val delta = DeltaBufferRDT(n.toString, RCounterInterface.zero).increment().deltaBuffer.head
        c.applyDelta(delta)
    }
  }

  @Benchmark
  def value(): Int = counter.value

  @Benchmark
  def fresh(): DeltaBufferRDT[RCounter] = counter.fresh()

  @Benchmark
  def increment(): DeltaBufferRDT[RCounter] = counter.increment()

  @Benchmark
  def decrement(): DeltaBufferRDT[RCounter] = counter.decrement()

  @Benchmark
  def reset(): DeltaBufferRDT[RCounter] = counter.reset()
}
