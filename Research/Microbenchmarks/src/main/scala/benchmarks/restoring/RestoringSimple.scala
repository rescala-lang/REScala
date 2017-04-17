package benchmarks.restoring

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Size, Step}
import org.openjdk.jmh.annotations._
import rescala.engine.Engine
import rescala.propagation.Turn
import rescala.reactives.{Evt, Signal, Var}
import rescala.restore.{ReStoringEngine, Storing}

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
  var result: List[Any] = _

  @Setup
  def setup(size: Size, engineParam: EngineParam[S]) = {
    engine = engineParam.engine
    source = engine.Evt[Int]()
    result = Nil
    if (size.size <= 0) result = List(source.map(_+1))
    for (_ <- Range(0, size.size)) {
      result = source.count :: result
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

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class RestoringSnapshot[S <: rescala.graph.Struct] {

  var snapshot: Seq[(String, Storing)] = _

  def build(implicit engine: ReStoringEngine, size: Int) = {
    val source = engine.Evt[Int]()
    var i = 0
    while (i < size) {
      source.count.map(_+1).map(_+1)
      i += 1
    }
    source
  }

  @Setup
  def setup(size: Size) = {
    val engine = new ReStoringEngine()
    val source = build(engine, size.size)
    source.fire(10)(engine)
    source.fire(20)(engine)
    snapshot = engine.snapshot().toSeq
  }

  @Benchmark
  def fresh(size: Size): Unit = build(new ReStoringEngine(), size.size)

  @Benchmark
  def restored(size: Size): Unit = build(new ReStoringEngine(restoreFrom = snapshot), size.size)


}
