package benchmarks.simple

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Size, Step, Workload}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.{BenchmarkParams, ThreadParams}
import rescala.propagation.Turn
import rescala.engines.{Engine, Engines}
import rescala.reactives.{Signal, SignalImpl, Signals}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(4)
@State(Scope.Benchmark)
class ReverseFan[S <: rescala.graph.Struct] {

  implicit var engine: Engine[S, Turn[S]] = _

  var sources: Array[rescala.reactives.VarImpl[Int, S]] = _
  var result: SignalImpl[Int, S] = _
  var isManual: Boolean = false

  @Setup
  def setup(params: BenchmarkParams, size: Size, step: Step, engineParam: EngineParam[S], work: Workload) = {
    engine = engineParam.engine
    val localEngine = engine
    import localEngine._
    val threads = params.getThreads
    sources = Array.fill(threads)(Var(step.get()))
    val intermediate = sources.map(_.map { v => {work.consume(); v + 1} })
    result = Signals.static(intermediate.toSeq: _*) { t => val r = intermediate.foldLeft(0)((a, v) => v.get(t) + a); work.consumeSecondary(); r }
    if (engine == Engines.unmanaged) isManual = true

  }

  @Benchmark
  def run(step: Step, params: ThreadParams): Unit =
    if (isManual) synchronized {sources(params.getThreadIndex).set(step.run())}
    else sources(params.getThreadIndex).set(step.run())
}
