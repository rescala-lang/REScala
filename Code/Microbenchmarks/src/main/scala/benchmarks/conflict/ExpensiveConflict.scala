package benchmarks.conflict

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import benchmarks.{EngineParam, Workload}
import org.openjdk.jmh.annotations._
import rescala.interface.RescalaInterface

@AuxCounters
@State(Scope.Thread)
class EvaluationCounter {
  var tried: Int     = _
  var succeeded: Int = _

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

  var engine: RescalaInterface = _
  final lazy val stableEngine  = engine
  import stableEngine._

  var cheapSource: Var[Int]     = _
  var expensiveSource: Var[Int] = _
  var result: Signal[Int]       = _
  var tried: Int                = _

  @Setup(Level.Iteration)
  def setup(engineParam: EngineParam, work: Workload) = {
    this.engine = engineParam.engine
    tried = 0
    cheapSource = Var(input.incrementAndGet())
    expensiveSource = Var(input.incrementAndGet())
    val expensive = expensiveSource.map { v =>
      tried += 1; val r = v + 1; work.consume(); r
    }
    result = Signals.lift(expensive, cheapSource)(_ + _).map { v =>
      val r = v + 1; work.consumeSecondary(); r
    }
  }

  @Benchmark
  @Group("g")
  @GroupThreads(1)
  def cheap() = {
    cheapSource.set(input.incrementAndGet())(scheduler, ScopeSearch.fromSchedulerImplicit(scheduler))
  }

  @Benchmark
  @Group("g")
  @GroupThreads(1)
  def expensive(counter: EvaluationCounter) = {
    expensiveSource.set(input.incrementAndGet())(scheduler, ScopeSearch.fromSchedulerImplicit(scheduler))
    counter.tried += tried
    counter.succeeded += 1
    tried = 0
  }

}
