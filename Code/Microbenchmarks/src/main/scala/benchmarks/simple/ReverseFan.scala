package benchmarks.simple

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Size, Step, Workload}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.{BenchmarkParams, ThreadParams}
import rescala.Schedulers

import rescala.interface.RescalaInterface

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(4)
@State(Scope.Benchmark)
class ReverseFan {

  var engine: RescalaInterface = _
  lazy val stableEngine        = engine
  import stableEngine._

  var sources: Array[Var[Int]] = _
  var result: Signal[Int]      = _
  var isManual: Boolean        = false

  @Setup
  def setup(params: BenchmarkParams, size: Size, step: Step, engineParam: EngineParam, work: Workload) = {
    engine = engineParam.engine
    sources = Array.fill(16)(Var(step.get()))
    val intermediate = sources.map(_.map { v => { work.consume(); v + 1 } })
    result = Signals.lift(intermediate.toSeq) { values => work.consumeSecondary(); values.sum }
    if (engine == Schedulers.unmanaged) isManual = true

  }

  @Benchmark
  def run(step: Step, params: ThreadParams): Unit =
    if (isManual) synchronized { sources(params.getThreadIndex).set(step.run())(scheduler) }
    else sources(params.getThreadIndex).set(step.run())(scheduler)
}
