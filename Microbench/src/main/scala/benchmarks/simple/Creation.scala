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
@State(Scope.Benchmark)
class Creation[S <: rescala.graph.Spores] {

  implicit var engine: Engine[S, Turn[S]] = _

  var sourceSignal: Signal[String, S] = _
  var sourceEvent: Event[String, S] = _

  @Setup
  def setup(params: BenchmarkParams, work: Workload, engineParam: EngineParam[S], threadState: CreationStateThread[S]) = {
    engine = engineParam.engine
    sourceSignal = Var[String, S]("")
    sourceEvent = Evt[String, S]()
    threadState.signal = sourceSignal.map(_ => "")
    threadState.event = sourceEvent.map(_ => "")
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
  def `derived signal`(threadState: CreationStateThread[S]): Signal[String, S] = {
    threadState.signal.map(identity)
  }

  @Benchmark
  def `derived event`(threadState: CreationStateThread[S]): Event[String, S] = {
    threadState.event.map(identity)
  }

  @Benchmark
  def `signal fanout`(size: Size, threadState: CreationStateThread[S]): Seq[Signal[String, S]] = {
    Range(0,size.size).map(_ => threadState.signal.map(identity))
  }

}

@State(Scope.Thread)
class CreationStateThread[S <: rescala.graph.Spores] {
  var signal: Signal[String, S] = _
  var event: Event[String, S] = _
}
