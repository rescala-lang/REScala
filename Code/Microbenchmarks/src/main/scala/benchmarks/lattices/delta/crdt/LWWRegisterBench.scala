package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.CContext.DietMapCContext
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

  var empty: LWWRegister[Int, DietMapCContext] = _
  var full: LWWRegister[Int, DietMapCContext]  = _

  @Setup
  def setup(): Unit = {
    empty = LWWRegister[Int, DietMapCContext]("a")
    full = LWWRegister[Int, DietMapCContext]("b").write(0)
  }

  @Benchmark
  def readEmpty(): Option[Int] = empty.read

  @Benchmark
  def readFull(): Option[Int] = full.read

  @Benchmark
  def writeEmpty(): LWWRegister[Int, DietMapCContext] = empty.write(1)

  @Benchmark
  def writeFull(): LWWRegister[Int, DietMapCContext] = full.write(1)

  @Benchmark
  def mapEmpty(): LWWRegister[Int, DietMapCContext] = empty.map(_ + 1)

  @Benchmark
  def mapFull(): LWWRegister[Int, DietMapCContext] = full.map(_ + 1)

  @Benchmark
  def clearEmpty(): LWWRegister[Int, DietMapCContext] = empty.clear()

  @Benchmark
  def clearFull(): LWWRegister[Int, DietMapCContext] = full.clear()
}
