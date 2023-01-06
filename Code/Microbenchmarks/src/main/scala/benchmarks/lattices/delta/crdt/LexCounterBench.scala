package benchmarks.lattices.delta.crdt

import kofre.decompose.interfaces.LexCounterInterface.LexCounter
import org.openjdk.jmh.annotations._
import kofre.decompose.interfaces.LexCounterInterface
import kofre.deprecated.containers.DeltaBufferRDT

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

  var counter: DeltaBufferRDT[LexCounter] = _

  @Setup
  def setup(): Unit = {
    counter = (1 until numReplicas).foldLeft(DeltaBufferRDT("0", LexCounterInterface.empty).inc()) {
      case (c, n) =>
        val delta = DeltaBufferRDT(n.toString, LexCounterInterface.empty).inc().deltaBuffer.head
        c.applyDelta(delta)
    }
  }

  @Benchmark
  def value(): Int = counter.value

  @Benchmark
  def inc(): DeltaBufferRDT[LexCounter] = counter.inc()

  @Benchmark
  def dec(): DeltaBufferRDT[LexCounter] = counter.dec()
}
