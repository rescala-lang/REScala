package benchmarks.lattices.delta.crdt

import kofre.causality.CausalContext
import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.crdt.reactive.MVRegister

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

  var reg: MVRegister[Int, CausalContext] = _

  @Setup
  def setup(): Unit = {
    reg = (0 until numWrites).foldLeft(MVRegister[Int, CausalContext]("-1")) {
      case (r, i) =>
        val delta = MVRegister[Int, CausalContext](i.toString).write(i).deltaBuffer.head
        r.applyDelta(delta)
    }
  }

  @Benchmark
  def read(): Set[Int] = reg.read

  @Benchmark
  def write(): MVRegister[Int, CausalContext] = reg.write(-1)

  @Benchmark
  def clear(): MVRegister[Int, CausalContext] = reg.clear()
}
