package benchmarks

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.turns.{Engine, Turn}
import rescala._

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class Creation {

  implicit var engine: Engine[Turn] = _

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
  def `var and derived signal`(): Signal[String] = {
    val v1 = Var("")
    v1.map(identity)
  }

  @Benchmark
  def `evt and derived event`(): Event[String] = {
    val e1 = Evt[String]()
    e1.map(identity)
  }

  @Benchmark
  def `signal fanout`(): Seq[Signal[String]] = {
    val v1 = Var("")
    Range(0,100).map(_ => v1.map(identity))
  }

}
