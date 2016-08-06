package benchmarks.dynamic

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Step}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.propagation.Turn
import rescala.engines.{Engine, Engines}
import rescala.reactives.{Signals, Var, VarImpl}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class SingleSwitch[S <: rescala.graph.Struct] {

  implicit var engine: Engine[S, Turn[S]] = _

  var source: VarImpl[Int, S] = _

  var isManual: Boolean = false

  @Setup
  def setup(params: BenchmarkParams, step: Step, engineParam: EngineParam[S]) = {
    engine = engineParam.engine
    source = Var(step.get())
    val d1 = Var("true")
    val d2 = Var("false")
    val dynamic = Signals.dynamic(source) { t =>
      if (step.test(source(t))) d1(t) else d2(t)
    }

    if (engine == Engines.unmanaged) isManual = true

  }

  @Benchmark
  def run(step: Step): Unit =
    if (isManual) synchronized {source.set(step.run())}
    else source.set(step.run())
}
