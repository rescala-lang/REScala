package benchmarks

import java.util.concurrent.TimeUnit

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
class Dynamic {

  implicit var engine: Engine[Turn] = _

  var source: Var[Boolean] = _

  var current: Boolean = false

  @Setup
  def setup(params: BenchmarkParams, work: Workload, engineParam: EngineParam) = {
    engine = engineParam.engine
    source = Var(current)
    val d1 = Var("true")
    val d2 = Var("false")
    val dynamic = Signals.dynamic(source) { t =>
      if (source(t)) d1(t) else d2(t)
    }
  }

  @Benchmark
  def run(): Unit = {
    current = !current
    source.set(current)
  }
}
