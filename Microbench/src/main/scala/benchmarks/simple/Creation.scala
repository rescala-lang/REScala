package benchmarks.simple

import java.util.concurrent.TimeUnit

import benchmarks.{Size, EngineParam, Workload}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.turns.{Engine, Turn}
import rescala.{Event, Evt, Signal, Var}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Thread)
class Creation[S <: rescala.graph.Spores] {

  implicit var engine: Engine[S, Turn[S]] = _

  var signal: Signal[String, S] = _
  var event: Event[String, S] = _

  @Setup
  def setup(params: BenchmarkParams, work: Workload, engineParam: EngineParam[S], benchState: CreationStateBench[S]) = {
    engine = engineParam.engine
    signal = benchState.sourceSignal.map(_ => "")
    event = benchState.sourceEvent.map(_ => "")
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
  def `derived signal`(): Signal[String, S] = {
    signal.map(identity)
  }

  @Benchmark
  def `derived event`(): Event[String, S] = {
    event.map(identity)
  }

  @Benchmark
  def `signal fanout`(size: Size): Seq[Signal[String, S]] = {
    Range(0,size.size).map(_ => signal.map(identity))
  }

}

@State(Scope.Benchmark)
class CreationStateBench[S <: rescala.graph.Spores] {
  var sourceSignal: Signal[String, S] = _
  var sourceEvent: Event[String, S] = _
  @Setup
  def setup(engineParam: EngineParam[S]) = {
    implicit val engine: Engine[S, Turn[S]] = engineParam.engine
    sourceSignal = Var[String, S]("")
    sourceEvent = Evt[String, S]()
  }

}
