package benchmarks.lattices.delta.crdt.basic

import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.CContext.DietMapCContext
import rescala.extra.lattices.delta.crdt.reactive.LWW

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class LWWBench {

  var empty: LWW[Int, DietMapCContext] = _
  var full: LWW[Int, DietMapCContext]  = _

  @Setup
  def setup(): Unit = {
    empty = LWW[Int, DietMapCContext]("a")
    full = LWW[Int, DietMapCContext]("b").write(0)
  }

  @Benchmark
  def readEmpty(): Option[Int] = empty.read

  @Benchmark
  def readFull(): Option[Int] = full.read

  @Benchmark
  def writeEmpty(): LWW[Int, DietMapCContext] = empty.write(1)

  @Benchmark
  def writeFull(): LWW[Int, DietMapCContext] = full.write(1)

  @Benchmark
  def mapEmpty(): LWW[Int, DietMapCContext] = empty.map(_ + 1)

  @Benchmark
  def mapFull(): LWW[Int, DietMapCContext] = full.map(_ + 1)

  @Benchmark
  def clearEmpty(): LWW[Int, DietMapCContext] = empty.clear()

  @Benchmark
  def clearFull(): LWW[Int, DietMapCContext] = full.clear()
}
