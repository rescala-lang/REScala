package benchmarks.basic

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Workload}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.interface.RescalaInterface

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class TurnCreation {

  var engine: RescalaInterface = _
  lazy val stableEngine        = engine

  @Setup
  def setup(params: BenchmarkParams, work: Workload, engineParam: EngineParam) = {
    engine = engineParam.engine
  }

  @Benchmark
  def run(): Any = {
    engine.transaction()(identity)
  }

}
