package benchmarks.simple

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Step}
import org.openjdk.jmh.annotations._
import rescala.turns.{Engine, Turn}
import rescala.{Signal, Signals, Var}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Thread)
class SimplePhil[S <: rescala.graph.Spores] {

  implicit var engine: Engine[S, Turn[S]] = _

  var phil: Var[Int, S] = _
  var vision: Signal[Boolean, S] = _

  def buildPhil(): (Var[Int, S], Signal[Boolean, S]) = {
    val p: Var[Int, S] = engine.Var(0)
    val f1, f2 = p.map(identity)
    val v = Signals.lift(f1, f2) {(a, b) => a == b}
    (p, v)
  }

  @Setup
  def setup(engineParam: EngineParam[S]) = {
    engine = engineParam.engine
    val (p, v) = build()
    phil = p
    vision = v
  }

  @Benchmark
  def propagate(step: Step): Unit = phil.set(step.run())

  @Benchmark
  def build(): (Var[Int, S], Signal[Boolean, S]) = buildPhil()

  @Benchmark
  def buildAndPropagate(step: Step): Unit = {
    val (p, v) = buildPhil()
    p.set(10)
  }
}
