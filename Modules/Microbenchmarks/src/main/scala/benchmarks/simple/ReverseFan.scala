package benchmarks.simple

import benchmarks.{EngineParam, Step, Workload}
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.ThreadParams
import reactives.operator.Interface

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(4)
@State(Scope.Benchmark)
class ReverseFan {

  var engine: Interface       = scala.compiletime.uninitialized
  final lazy val stableEngine = engine
  import stableEngine._

  var sources: Array[Var[Int]] = scala.compiletime.uninitialized
  var result: Signal[Int]      = scala.compiletime.uninitialized
  var isManual: Boolean        = false

  @Setup
  def setup(step: Step, engineParam: EngineParam, work: Workload) = {
    engine = engineParam.engine
    sources = Array.fill(16)(Var(step.get()))
    val intermediate = sources.map(_.map { v => { work.consume(); v + 1 } })
    result = Signal.lift(intermediate.toSeq) { values =>
      work.consumeSecondary(); values.sum
    }
    if (engine.scheduler == reactives.scheduler.LevelbasedVariants.unmanaged) isManual = true

  }

  @Benchmark
  def run(step: Step, params: ThreadParams): Unit =
    if (isManual) synchronized {
      sources(params.getThreadIndex).set(step.run())(using scheduler)
    }
    else sources(params.getThreadIndex).set(step.run())(using scheduler)
}
