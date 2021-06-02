package benchmarks.simple

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Workload}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.interface.RescalaInterface
import rescala.operator._

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class Creation {

  var engine: RescalaInterface = _
  lazy val stableEngine        = engine
  import stableEngine._

  @Setup
  def setup(params: BenchmarkParams, work: Workload, engineParam: EngineParam) = {
    engine = engineParam.engine
  }

  @Benchmark
  def `var`(): Var[String] = {
    Var("")
  }

  @Benchmark
  def `evt`(): Evt[String] = {
    Evt[String]()
  }

  @Benchmark
  def `derived signal`(): Signal[String] = {
    Var("").map(identity)
  }

  @Benchmark
  def `derived event`(): Event[String] = {
    Evt[String]().map(identity)
  }

}
