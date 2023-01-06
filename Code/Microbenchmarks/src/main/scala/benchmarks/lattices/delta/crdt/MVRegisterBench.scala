package benchmarks.lattices.delta.crdt

import kofre.decompose.interfaces.MVRegister
import kofre.deprecated.containers.DeltaBufferRDT
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

  var reg: DeltaBufferRDT[MVRegister[Int]] = _

  @Setup
  def setup(): Unit = {
    reg = (0 until numWrites).foldLeft(DeltaBufferRDT("-1", MVRegister.empty[Int])) {
      case (r, i) =>
        val delta = DeltaBufferRDT(i.toString, MVRegister.empty[Int]).write(i).deltaBuffer.head
        r.applyDelta(delta)
    }
  }

  @Benchmark
  def read(): Set[Int] = reg.read

  @Benchmark
  def write(): DeltaBufferRDT[MVRegister[Int]] = reg.write(-1)

  @Benchmark
  def clear(): DeltaBufferRDT[MVRegister[Int]] = reg.clear()
}
