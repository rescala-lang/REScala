package benchmarks.dynamic

import benchmarks.{EngineParam, Size, Step, Workload}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.{BenchmarkParams, ThreadParams}
import reactives.operator.Interface

import java.util.concurrent.TimeUnit
import scala.collection.immutable.Range

/** creates a chain per thread, and connects the result dynamically with the chain of another
  * sources -> chain … chain -> result <- dynamically chain of other source
  */
@State(Scope.Benchmark)
class StackState {

  var engine: Interface       = scala.compiletime.uninitialized
  final lazy val stableEngine = engine
  import stableEngine._

  var sources: Array[Var[Int]]     = scala.compiletime.uninitialized
  var results: Array[Signal[Int]]  = scala.compiletime.uninitialized
  var dynamics: Array[Signal[Int]] = scala.compiletime.uninitialized
  var isManual: Boolean            = false

  @Setup(Level.Iteration)
  def setup(params: BenchmarkParams, eParam: EngineParam, work: Workload, size: Size, step: Step) = {
    engine = eParam.engine
    val threads = params.getThreads
    if (engine.global.scheduler == reactives.scheduler.LevelbasedVariants.unmanaged) { isManual = true }
    sources = Range(0, threads).map(_ => Var(0)).toArray
    results = sources.map { source =>
      var cur: Signal[Int] = source
      for (_ <- Range(0, size.size)) { cur = cur.map(1.+) }
      cur.map { x => { work.consume(); x } }
    }

    dynamics = results.zipWithIndex.map {
      case (r, i) =>
        Signal.dynamic {
          val v   = r.value
          val idx = i + (if (step.test(v)) 2 else 1)
          results(idx % threads).value
        }
    }
  }
}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 100, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(8)
class Stacks {

  @Benchmark
  def run(state: StackState, step: Step, params: ThreadParams) = {
    import state.stableEngine._
    if (state.isManual)
      state.synchronized {
        val index = params.getThreadIndex % params.getThreadCount
        state.sources(index).set(step.run())
        state.dynamics(index).readValueOnce
      }
    else {
      val index = params.getThreadIndex % params.getThreadCount
      state.sources(index).set(step.run())
      state.dynamics(index).readValueOnce
    }
  }

}
