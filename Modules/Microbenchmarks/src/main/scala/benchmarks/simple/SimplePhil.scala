package benchmarks.simple

import benchmarks.EngineParam
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Thread)
class SimplePhil {

  import benchmarks.philosophers.PhilosopherTable.*

  var engine: reactives.default.type = scala.compiletime.uninitialized
  final lazy val stableEngine        = engine
  import stableEngine.*

  var phil: Var[Philosopher] = scala.compiletime.uninitialized
  var vision: Signal[Vision] = scala.compiletime.uninitialized

  def buildPhil(): (Var[Philosopher], Signal[Vision]) = {
    val p: Var[Philosopher] = Var(Thinking)
    val f1, f2              = p.map(s => if s == Thinking then Free else Taken("me"))
    val v                   = Signal.lift(f1, f2) { calcVision("me") }
    (p, v)
  }

  @Setup
  def setup(engineParam: EngineParam) = {
    engine = engineParam.engine
    val (p, v) = build()
    phil = p
    vision = v
  }

  @Benchmark
  def propagate(): Unit = {
    phil.set(Eating)
    phil.set(Thinking)
  }

  @Benchmark
  def build(): (Var[Philosopher], Signal[Vision]) = buildPhil()

  @Benchmark
  def buildAndPropagate(): Unit = {
    val (p, v) = buildPhil()
    p.set(Eating)
    p.set(Thinking)
  }
}
