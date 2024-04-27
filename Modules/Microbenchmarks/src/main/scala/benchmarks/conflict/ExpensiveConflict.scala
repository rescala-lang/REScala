package benchmarks.conflict

import benchmarks.{EngineParam, Workload}
import org.openjdk.jmh.annotations.*
import reactives.operator.Interface

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

@AuxCounters
@State(Scope.Thread)
class EvaluationCounter {
  var tried: Int     = scala.compiletime.uninitialized
  var succeeded: Int = scala.compiletime.uninitialized

  @Setup(Level.Iteration)
  def reset() = {
    tried = 0
    succeeded = 0
  }

}

@State(Scope.Group)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(2)
class ExpensiveConflict {

  var input: AtomicInteger = new AtomicInteger(0)

  var engine: Interface       = scala.compiletime.uninitialized
  final lazy val stableEngine = engine
  import stableEngine.*

  var cheapSource: Var[Int]     = scala.compiletime.uninitialized
  var expensiveSource: Var[Int] = scala.compiletime.uninitialized
  var result: Signal[Int]       = scala.compiletime.uninitialized
  var tried: Int                = scala.compiletime.uninitialized

  @Setup(Level.Iteration)
  def setup(engineParam: EngineParam, work: Workload) = {
    this.engine = engineParam.engine
    tried = 0
    cheapSource = Var(input.incrementAndGet())
    expensiveSource = Var(input.incrementAndGet())
    val expensive = expensiveSource.map { v =>
      tried += 1; val r = v + 1; work.consume(); r
    }
    result = Signal.lift(expensive, cheapSource)(_ + _).map { v =>
      val r = v + 1; work.consumeSecondary(); r
    }
  }

  @Benchmark
  @Group("g")
  @GroupThreads(1)
  def cheap() = {
    cheapSource.set(input.incrementAndGet())
  }

  @Benchmark
  @Group("g")
  @GroupThreads(1)
  def expensive(counter: EvaluationCounter) = {
    expensiveSource.set(input.incrementAndGet())
    counter.tried += tried
    counter.succeeded += 1
    tried = 0
  }

}
