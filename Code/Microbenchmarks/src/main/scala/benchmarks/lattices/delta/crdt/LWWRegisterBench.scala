package benchmarks.lattices.delta.crdt

import kofre.causality.CausalContext
import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.crdt.reactive.LWWRegister

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class LWWRegisterBench {

  var empty: LWWRegister[Int, CausalContext] = _
  var full: LWWRegister[Int, CausalContext]  = _

  @Setup
  def setup(): Unit = {
    empty = LWWRegister[Int, CausalContext]("a")
    full = LWWRegister[Int, CausalContext]("b").write(0)
  }

  @Benchmark
  def readEmpty(): Option[Int] = empty.read

  @Benchmark
  def readFull(): Option[Int] = full.read

  @Benchmark
  def writeEmpty(): LWWRegister[Int, CausalContext] = empty.write(1)

  @Benchmark
  def writeFull(): LWWRegister[Int, CausalContext] = full.write(1)

  @Benchmark
  def mapEmpty(): LWWRegister[Int, CausalContext] = empty.map(_ + 1)

  @Benchmark
  def mapFull(): LWWRegister[Int, CausalContext] = full.map(_ + 1)

  @Benchmark
  def clearEmpty(): LWWRegister[Int, CausalContext] = empty.clear()

  @Benchmark
  def clearFull(): LWWRegister[Int, CausalContext] = full.clear()
}
