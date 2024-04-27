package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import rdts.base.Uid.asId
import rdts.datatypes.alternatives.ResettableCounter
import rdts.dotted.Dotted
import rdts.syntax.LocalUid

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
  var numReplicas: Int = scala.compiletime.uninitialized

  var counter: DeltaBufferDotted[ResettableCounter] = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    counter =
      (1 until numReplicas).foldLeft(NamedDeltaBuffer.dotted("0", ResettableCounter.zero).increment(using "0".asId)()) {
        case (c, n) =>
          given rid: LocalUid = n.toString.asId
          val delta                 = Dotted(ResettableCounter.zero).increment()
          c.applyDelta(rid.uid, delta)
      }
  }

  @Benchmark
  def value(): Int = counter.value

  @Benchmark
  def fresh(): DeltaBufferDotted[ResettableCounter] = counter.fresh(using counter.replicaID)()

  @Benchmark
  def increment(): DeltaBufferDotted[ResettableCounter] = counter.increment(using counter.replicaID)()

  @Benchmark
  def decrement(): DeltaBufferDotted[ResettableCounter] = counter.decrement(using counter.replicaID)()

  @Benchmark
  def reset(): DeltaBufferDotted[ResettableCounter] = counter.reset()
}
