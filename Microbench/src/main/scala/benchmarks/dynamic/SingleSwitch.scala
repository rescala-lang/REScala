package benchmarks.dynamic

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Step}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.turns.{Engine, Turn}
import rescala.{Signals, Var}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class SingleSwitch[S <: rescala.graph.Spores] {

  implicit var engine: Engine[S, Turn[S]] = _

  var source: Var[Long, S] = _

  @Setup
  def setup(params: BenchmarkParams, step: Step, engineParam: EngineParam[S]) = {
    engine = engineParam.engine
    source = Var(step.get())
    val d1 = Var("true")
    val d2 = Var("false")
    val dynamic = Signals.dynamic(source) { t =>
      if (step.test(source(t))) d1(t) else d2(t)
    }
  }

  @Benchmark
  def run(step: Step): Unit = source.set(step.run())
}
