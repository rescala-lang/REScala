package benchmarks

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.turns.{Engine, Ticket, Turn}
import rescala.{Event, Evt, Signal, Var}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class Creation[S <: rescala.graph.State] {

  implicit var engine: Engine[S, Turn[S]] = _

  @Setup
  def setup(params: BenchmarkParams, work: Workload, engineParam: EngineParam[S]) = {
    engine = engineParam.engine
  }

  @Benchmark
  def `var`(): Var[String, S] = {
    engine.Var("")
  }

  @Benchmark
  def `evt`(): Evt[String, S] = {
    engine.Evt[String]()
  }

  @Benchmark
  def `var and derived signal`(): Signal[String, S] = {
    val v1 = engine.Var("")
    v1.map(identity)(Ticket.dynamic(engine))
  }

  @Benchmark
  def `evt and derived event`(): Event[String, S] = {
    val e1 = engine.Evt[String]()
    e1.map(identity)
  }

  @Benchmark
  def `signal fanout`(): Seq[Signal[String, S]] = {
    val v1 = Var("")
    Range(0,100).map(_ => v1.map(identity))
  }

}
