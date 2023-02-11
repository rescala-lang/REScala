package benchmarks.errorprop

import benchmarks.{EngineParam, Size, Step, Workload}
import org.openjdk.jmh.annotations._
import rescala.interface.RescalaInterface

import java.util.concurrent.TimeUnit
import scala.util.Try

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class MonadicErrors {

  var engine: RescalaInterface = _
  final lazy val stableEngine  = engine
  import stableEngine._

  var fire: Int => Unit       = _
  var finalresult: Event[Any] = _

  @Param(Array("true", "false"))
  var isMonadic: Boolean = _

  @Setup
  def setup(size: Size, engineParam: EngineParam, work: Workload) = {
    engine = engineParam.engine
    if (isMonadic) {
      val source                  = Evt[Try[Int]]()
      var result: Event[Try[Int]] = source
      for (_ <- Range(1, size.size)) {
        result = result.map { (t: Try[Int]) =>
          t.map { v =>
            val r = v + 1; work.consume(); r
          }
        }
      }
      finalresult = result
      fire = i => source.fire(Try { i })
    } else {
      val source             = Evt[Int]()
      var result: Event[Int] = source
      for (_ <- Range(1, size.size)) {
        result = result.map { v =>
          val r = v + 1; work.consume(); r
        }
      }
      finalresult = result
      fire = source.fire
    }
  }

  @Benchmark
  def run(step: Step): Unit = fire(step.run())
}
