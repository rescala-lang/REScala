package benchmarks.simple

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Step}
import org.openjdk.jmh.annotations._
import rescala.core.{Scheduler, Struct}
import rescala.reactives._

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Thread)
class SimplePhil[S <: Struct] {

  import benchmarks.philosophers.PhilosopherTable._

  implicit var engine: Scheduler[S] = _

  var phil: Var[Philosopher, S] = _
  var vision: Signal[Vision, S] = _

  def buildPhil(): (Var[Philosopher, S], Signal[Vision, S]) = {
    val p: Var[Philosopher, S] = engine.Var(Thinking)
    val f1, f2 = p.map(s => if (s == Thinking) Free else Taken("me"))
    val v = Signals.lift(f1, f2) {calcVision("me")}
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
  def propagate(): Unit = {
    phil.set(Eating)
    phil.set(Thinking)
  }

  @Benchmark
  def build(): (Var[Philosopher, S], Signal[Vision, S]) = buildPhil()

  @Benchmark
  def buildAndPropagate(step: Step): Unit = {
    val (p, v) = buildPhil()
    p.set(Eating)
    p.set(Thinking)
  }
}
