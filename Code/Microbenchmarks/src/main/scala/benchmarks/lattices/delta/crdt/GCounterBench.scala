package benchmarks.lattices.delta.crdt

import kofre.decompose.interfaces.GCounterInterface.GCounter
import kofre.decompose.interfaces.GCounterInterface.GCounter.GCounterSyntax
import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.crdt.reactive.ReactiveDeltaCRDT

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

  var counter: ReactiveDeltaCRDT[GCounter] = _

  @Setup
  def setup(): Unit = {
    counter = (1 until numReplicas).foldLeft(ReactiveDeltaCRDT[GCounter]("0").inc()) {
      case (c, n) =>
        val delta = ReactiveDeltaCRDT[GCounter](n.toString).inc().deltaBuffer.head
        c.applyDelta(delta)
    }
  }

  @Benchmark
  def value(): Int = counter.value

  @Benchmark
  def inc(): ReactiveDeltaCRDT[GCounter] = counter.inc()
}
