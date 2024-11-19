package benchmarks.dynamic

import benchmarks.{EngineParam, Step}
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class SingleSwitch {

  var engine: reactives.default.type = scala.compiletime.uninitialized
  final lazy val stableEngine        = engine
  import stableEngine.*

  var source: Var[Int] = scala.compiletime.uninitialized

  var isManual: Boolean = false

  @Setup
  def setup(step: Step, engineParam: EngineParam): Unit = {
    engine = engineParam.engine
    source = Var(step.get())
    val d1 = Var("true")
    val d2 = Var("false")
    Signal.dynamic {
      if step.test(source.value) then d1.value else d2.value
    }

    if reactives.SelectedScheduler.candidate.scheduler == reactives.scheduler.LevelbasedVariants.unmanaged then
      isManual = true

  }

  @Benchmark
  def run(step: Step): Unit =
    if isManual then synchronized { source.set(step.run()) } else source.set(step.run())
}
