package benchmarks.simple

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Size, Step}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.engines.Engine
import rescala.propagation.Turn
import rescala.reactives._

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Thread)
class Fan[S <: rescala.graph.Struct] {

  implicit var engine: Engine[S, Turn[S]] = _

  var source: Var[Int, S] = _
  var result: Signal[Int, S] = _

  @Setup
  def setup(params: BenchmarkParams, size: Size, step: Step, engineParam: EngineParam[S]) = {
    engine = engineParam.engine
    source = Var(step.run())
    val res = for (_ <- Range(0, size.size)) yield {
      source.map(_ + 1)
    }
    result = Signals.static(res: _*) { t => res.foldLeft(0)((c, v) => v.get(t) + c) }
  }

  @Benchmark
  def run(step: Step): Unit = source.set(step.run())
}
