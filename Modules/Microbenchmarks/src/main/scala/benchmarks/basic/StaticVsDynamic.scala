package benchmarks.basic

import benchmarks.{EngineParam, Step}
import org.openjdk.jmh.annotations.*
import reactives.operator.Interface

import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReadWriteLock

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class StaticVsDynamic {

  var engine: Interface       = scala.compiletime.uninitialized
  final lazy val stableEngine = engine
  import stableEngine.*

  @Param(Array("true", "false"))
  var static: Boolean = scala.compiletime.uninitialized

  var source: Var[Boolean] = scala.compiletime.uninitialized
  var current: Boolean     = scala.compiletime.uninitialized
  var lock: ReadWriteLock  = scala.compiletime.uninitialized
  var a: Var[Int]          = scala.compiletime.uninitialized
  var b: Var[Int]          = scala.compiletime.uninitialized
  var res: Signal[Int]     = scala.compiletime.uninitialized

  @Setup
  def setup(engineParam: EngineParam): Unit = {
    engine = engineParam.engine
    current = true
    source = stableEngine.Var(current)
    a = stableEngine.Var { 10 }
    b = stableEngine.Var { 20 }

    res = if static then
      Signal.static(source, a, b) { st =>
        if st.dependStatic(source) then st.dependStatic(a) else st.dependStatic(b)
      }
    else Signal.dynamic { if source.value then a.value else b.value }

  }

  @Benchmark
  def switchOnly(): Unit = {
    current = !current
    source.set(current)
  }

  @Benchmark
  def aOnly(step: Step): Unit = {
    a.set(step.run())
  }
  @Benchmark
  def bOnly(step: Step): Unit = {
    b.set(step.run())
  }
}
