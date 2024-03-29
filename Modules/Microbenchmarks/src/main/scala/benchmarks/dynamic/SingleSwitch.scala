package benchmarks.dynamic

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Step}
import org.openjdk.jmh.annotations._
import reactives.operator.Interface

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class SingleSwitch {

  var engine: Interface       = scala.compiletime.uninitialized
  final lazy val stableEngine = engine
  import stableEngine._

  var source: Var[Int] = scala.compiletime.uninitialized

  var isManual: Boolean = false

  @Setup
  def setup(step: Step, engineParam: EngineParam): Unit = {
    engine = engineParam.engine
    source = Var(step.get())
    val d1 = Var("true")
    val d2 = Var("false")
    Signal.dynamic {
      if (step.test(source.value)) d1.value else d2.value
    }

    if (engine.global.scheduler == reactives.scheduler.LevelbasedVariants.unmanaged) isManual = true

  }

  @Benchmark
  def run(step: Step): Unit =
    if (isManual) synchronized { source.set(step.run()) }
    else source.set(step.run())
}
