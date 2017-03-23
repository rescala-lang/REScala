package benchmarks.restoring

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Size, Step}
import org.openjdk.jmh.annotations._
import rescala.engine.Engine
import rescala.propagation.Turn
import rescala.reactives.{Evt, Signal, Var}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class RestoringSimple[S <: rescala.graph.Struct] {

  implicit var engine: Engine[S, Turn[S]] = _

  var source: Evt[Int, S] = _
  var result: Signal[Int, S] = _

  @Setup
  def setup(size: Size, engineParam: EngineParam[S]) = {
    engine = engineParam.engine
    source = engine.Evt[Int]()
    for (_ <- Range(0, size.size)) {
        result = source.count
    }
  }

  @Benchmark
  def countMany(step: Step): Unit = source.fire(step.run())


}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class RestoringVar[S <: rescala.graph.Struct] {

  implicit var engine: Engine[S, Turn[S]] = _
  var sourceVar: Var[Int, S] = _

  @Setup
  def setup(engineParam: EngineParam[S]) = {
    engine = engineParam.engine
    sourceVar = engine.Var(-1)
  }

  @Benchmark
  def singleVar(step: Step): Unit = sourceVar() = step.run()
}
