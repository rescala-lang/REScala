package benchmarks.lattices.delta.crdt

import kofre.base.Lattice
import kofre.datatypes.MultiVersionRegister
import kofre.dotted.Dotted
import org.openjdk.jmh.annotations.*
import kofre.base.Uid.asId

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

  given Lattice[Int]                                    = math.max _
  var reg: DeltaBufferDotted[MultiVersionRegister[Int]] = _

  @Setup
  def setup(): Unit = {
    reg = (0 until numWrites).foldLeft(NamedDeltaBuffer.dotted("-1", MultiVersionRegister.empty[Int])) {
      case (r, i) =>
        given rid: kofre.syntax.ReplicaId = i.toString.asId
        val delta = Dotted(MultiVersionRegister.empty[Int]).write(i)
        r.applyDelta(rid.replicaId, delta)
    }
  }

  @Benchmark
  def read(): Set[Int] = reg.read

  @Benchmark
  def write(): DeltaBufferDotted[MultiVersionRegister[Int]] = reg.write(using reg.replicaID)(-1)

  @Benchmark
  def clear(): DeltaBufferDotted[MultiVersionRegister[Int]] = reg.clear()
}
