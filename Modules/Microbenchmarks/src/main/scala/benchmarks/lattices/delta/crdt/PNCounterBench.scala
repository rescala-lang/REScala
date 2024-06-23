package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import rdts.base.LocalUid
import rdts.base.Uid.asId
import rdts.datatypes.PosNegCounter

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
  var numReplicas: Int = scala.compiletime.uninitialized

  var counter: NamedDeltaBuffer[PosNegCounter] = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    counter = (1 until numReplicas).foldLeft(NamedDeltaBuffer("0".asId, PosNegCounter.zero).map(_.inc())) {
      case (c, n) =>
        given rid: LocalUid = rdts.base.Uid.predefined(n.toString)
        val delta                       = PosNegCounter.zero.inc()
        c.applyDelta(rid.uid, delta)
    }
  }

  @Benchmark
  def value(): Int = counter.state.value

  @Benchmark
  def inc(): NamedDeltaBuffer[PosNegCounter] = counter.map(_.inc())

  @Benchmark
  def dec(): NamedDeltaBuffer[PosNegCounter] = counter.map(_.dec())
}
