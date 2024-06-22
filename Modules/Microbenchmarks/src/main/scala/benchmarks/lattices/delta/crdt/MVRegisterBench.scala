package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import rdts.base.Lattice
import rdts.base.Uid.asId
import rdts.datatypes.contextual.MultiVersionRegister
import rdts.dotted.Dotted

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
  var numWrites: Int = scala.compiletime.uninitialized

  given Lattice[Int]                                    = math.max
  var reg: DeltaBufferDotted[MultiVersionRegister[Int]] = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    reg = (0 until numWrites).foldLeft(NamedDeltaBuffer.dotted("-1", MultiVersionRegister.empty[Int])) {
      case (r, i) =>
        given rid: rdts.syntax.LocalUid = i.toString.asId
        val delta                       = Dotted(MultiVersionRegister.empty[Int]).mod(_.write(i))
        r.applyDelta(rid.uid, delta)
    }
  }

  @Benchmark
  def read(): Set[Int] = reg.data.read

  @Benchmark
  def write(): DeltaBufferDotted[MultiVersionRegister[Int]] = reg.mod(_.write(using reg.replicaID)(-1))

  @Benchmark
  def clear(): DeltaBufferDotted[MultiVersionRegister[Int]] = reg.mod(_.clear())
}
