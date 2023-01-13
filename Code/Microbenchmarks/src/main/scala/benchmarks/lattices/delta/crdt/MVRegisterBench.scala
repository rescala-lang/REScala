package benchmarks.lattices.delta.crdt

import kofre.datatypes.MultiVersionRegister
import kofre.deprecated.containers.DeltaBufferDotted
import kofre.dotted.Dotted
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class MVRegisterBench {

  @Param(Array("0", "1", "10", "100", "1000"))
  var numWrites: Int = _

  var reg: DeltaBufferDotted[MultiVersionRegister[Int]] = _

  @Setup
  def setup(): Unit = {
    reg = (0 until numWrites).foldLeft(DeltaBufferDotted("-1", MultiVersionRegister.empty[Int])) {
      case (r, i) =>
        val delta = Dotted(MultiVersionRegister.empty[Int]).named(i.toString).write(i)
        r.applyDelta(delta.replicaId, delta.anon)
    }
  }

  @Benchmark
  def read(): Set[Int] = reg.read

  @Benchmark
  def write(): DeltaBufferDotted[MultiVersionRegister[Int]] = reg.write(-1)

  @Benchmark
  def clear(): DeltaBufferDotted[MultiVersionRegister[Int]] = reg.clear()
}
