package benchmarks.simple

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Workload}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala._
import rescala.turns.{Engine, Turn}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class SingleVar[S <: rescala.graph.Spores] {

  implicit var engine: Engine[S, Turn[S]] = _

  var source: Var[Boolean, S] = _
  var current: Boolean = _
  var illegalTurn: Turn[S] = _


  @Setup
  def setup(params: BenchmarkParams, work: Workload, engineParam: EngineParam[S]) = {
    engine = engineParam.engine
    current = false
    source = engine.Var(current)
    illegalTurn = engine.plan()(identity)
  }

  @Benchmark
  def write(): Unit = {
    current = !current
    source.set(current)
  }

  @Benchmark
  def read(): Boolean = {
    source.now
  }

  @Benchmark
  def readIllegal(): Boolean = {
    source.get(illegalTurn)
  }

}
