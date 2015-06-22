package benchmarks

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.turns.{Engine, Turn}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class TurnCreation {

  implicit var engine: Engine[Turn] = _


  @Setup
  def setup(params: BenchmarkParams, work: Workload, engineParam: EngineParam) = {
    engine = engineParam.engine
  }

  @Benchmark
  def run(): Turn = {
    engine.plan()(identity)
  }


}
