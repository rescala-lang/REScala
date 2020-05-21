package benchmarks.dynamic

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Step}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.Schedulers
import rescala.core.{Scheduler, Struct};import rescala.interface.RescalaInterface
import rescala.reactives.Var

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class SingleSwitch[S <: Struct] {

  var engine: RescalaInterface[S] = _
  implicit def scheduler: Scheduler[S] = engine.scheduler

  var source: Var[Int, S] = _

  var isManual: Boolean = false

  @Setup
  def setup(params: BenchmarkParams, step: Step, engineParam: EngineParam[S]): Unit = {
    engine = engineParam.engine
    val e = engine
    import e._
    source = Var(step.get())
    val d1 = Var("true")
    val d2 = Var("false")
    engine.Signal.dynamic {
      if (step.test(source())) d1() else d2()
    }

    if (engine.scheduler == Schedulers.unmanaged) isManual = true

  }

  @Benchmark
  def run(step: Step): Unit =
    if (isManual) synchronized {source.set(step.run())}
    else source.set(step.run())
}
