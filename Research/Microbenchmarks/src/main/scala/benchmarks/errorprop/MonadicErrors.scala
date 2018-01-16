package benchmarks.errorprop

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Size, Step, Workload}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.core.{Scheduler, Struct}
import rescala.reactives.Event

import scala.util.Try

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class MonadicErrors[S <: Struct] {

  implicit var engine: Scheduler[S] = _

  var fire: Int => Unit = _
  var finalresult: Event[Any, S] = _

  @Param(Array("true", "false"))
  var isMonadic: Boolean = _

  @Setup
  def setup(params: BenchmarkParams, size: Size, engineParam: EngineParam[S], work: Workload) = {
    engine = engineParam.engine
    if (isMonadic) {
      val source = engine.Evt[Try[Int]]
      var result: Event[Try[Int], S] = source
      for (_ <- Range(1, size.size)) {
        result = result.map { t: Try[Int] => t.map { v => val r = v + 1; work.consume(); r } }
      }
      finalresult = result
      fire = i => source.fire(Try{i})
    }
    else {
      val source = engine.Evt[Int]
      var result: Event[Int, S] = source
      for (_ <- Range(1, size.size)) {
        result = result.map {  v => val r = v + 1; work.consume(); r  }
      }
      finalresult = result
      fire = source.fire
    }
  }

  @Benchmark
  def run(step: Step): Unit = fire(step.run())
}

