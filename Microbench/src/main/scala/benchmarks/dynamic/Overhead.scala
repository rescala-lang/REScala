package benchmarks.dynamic

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import benchmarks.{EngineParam, Workload}
import org.openjdk.jmh.annotations._
import rescala._
import rescala.graph.Spores
import rescala.turns.{Engine, Turn}
import sun.misc.Contended

/**
 * base depends on one of two paths in support.
 * switches dynamically every Workload steps (0 never switch, 1 switch every step)
 *
 * base -> result
 * support -> a -?> result
 * support -> b -?> result
 */
@State(Scope.Group)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(2)
class Overhead[S <: Spores] {

  @Contended
  var baseInput: Int = 0
  @Contended
  var supportInput: Int = 0

  var supportSource: Var[Int, S] = _
  var baseSource: Var[Int, S] = _
  var result: Signal[Int, S] = _
  var engine: Engine[S, Turn[S]] = _

  @Setup(Level.Iteration)
  def setup(engine: EngineParam[S], work: Workload) = {
    this.engine = engine.engine
    implicit val e = this.engine
    supportSource = Var(supportInput)
    baseSource = Var(baseInput)
    val supportA = supportSource.map(_ + 1)
    val supportB = supportSource.map(_ + 1)
    val steps = work.work + 1
    result = Signals.dynamic(){t =>
      val v = baseSource(t)
      if(v % steps == 0) supportA(t)
      else supportB(t)
    }
  }

  @Benchmark
  @Group("g")
  @GroupThreads(1)
  def support() = {
    supportInput += 1
    supportSource.set(supportInput)(engine)
  }

  @Benchmark
  @Group("g")
  @GroupThreads(1)
  def base() = {
    baseInput += 1
    baseSource.set(baseInput)(engine)
  }


}
