package benchmarks.lattices.delta.crdt

import kofre.decompose.interfaces.MVRegisterInterface.MVRegister
import org.openjdk.jmh.annotations._
import kofre.decompose.interfaces.MVRegisterInterface.MVRegisterSyntax
import kofre.decompose.containers.ReactiveDeltaCRDT

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

  var reg: ReactiveDeltaCRDT[MVRegister[Int]] = _

  @Setup
  def setup(): Unit = {
    reg = (0 until numWrites).foldLeft(ReactiveDeltaCRDT[MVRegister[Int]]("-1")) {
      case (r, i) =>
        val delta = ReactiveDeltaCRDT[MVRegister[Int]](i.toString).write(i).deltaBuffer.head
        r.applyDelta(delta)
    }
  }

  @Benchmark
  def read(): Set[Int] = reg.read

  @Benchmark
  def write(): ReactiveDeltaCRDT[MVRegister[Int]] = reg.write(-1)

  @Benchmark
  def clear(): ReactiveDeltaCRDT[MVRegister[Int]] = reg.clear()
}
