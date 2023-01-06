package benchmarks.lattices.delta.crdt

import kofre.decompose.interfaces.MVRegisterInterface.MVRegister
import org.openjdk.jmh.annotations._
import kofre.decompose.interfaces.MVRegisterInterface.MVRegisterSyntax
import kofre.decompose.interfaces.MVRegisterInterface
import kofre.deprecated.containers.DeltaBufferRDT

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
    reg = (0 until numWrites).foldLeft(DeltaBufferRDT("-1", MVRegisterInterface.empty[Int])) {
      case (r, i) =>
        val delta = DeltaBufferRDT(i.toString, MVRegisterInterface.empty[Int]).write(i).deltaBuffer.head
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
