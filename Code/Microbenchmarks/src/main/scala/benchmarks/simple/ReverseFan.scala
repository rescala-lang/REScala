package benchmarks.simple

import benchmarks.{EngineParam, Step, Workload}
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.ThreadParams
import rescala.Schedulers
import rescala.core.ScopeSearch
import rescala.interface.RescalaInterface

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(4)
@State(Scope.Benchmark)
class ReverseFan {

  var engine: RescalaInterface = _
  final lazy val stableEngine  = engine
  import stableEngine._

  var sources: Array[Var[Int]] = _
  var result: Signal[Int]      = _
  var isManual: Boolean        = false

  @Setup
  def setup(step: Step, engineParam: EngineParam, work: Workload) = {
    engine = engineParam.engine
    sources = Array.fill(16)(Var(step.get()))
    val intermediate = sources.map(_.map { v => { work.consume(); v + 1 } })
    result = Signals.lift(intermediate.toSeq) { values =>
      work.consumeSecondary(); values.sum
    }
    if (engine == Schedulers.unmanaged) isManual = true

  }

  @Benchmark
  def run(step: Step, params: ThreadParams): Unit =
    if (isManual) synchronized {
      sources(params.getThreadIndex).set(step.run())(scheduler, ScopeSearch.fromSchedulerImplicit(scheduler))
    }
    else sources(params.getThreadIndex).set(step.run())(scheduler, ScopeSearch.fromSchedulerImplicit(scheduler))
}
