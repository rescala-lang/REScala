package benchmarks.dynamic

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Size, Step, Workload}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.{BenchmarkParams, ThreadParams}
import rescala.Engines
import rescala.core.{Scheduler, Struct}
import rescala.reactives._

import scala.collection.immutable.Range


/**
 * creates a chain per thread, and connects the result dynamically with the chain of another
 * sources -> chain … chain -> result <- dynamically chain of other source
 */
@State(Scope.Benchmark)
class StackState[S <: Struct] {

  var sources: Array[Var[Int, S]] = _
  var results: Array[Signal[Int, S]] = _
  var dynamics: Array[Signal[Int, S]] = _
  var engine: Scheduler[S] = _
  var isManual: Boolean = false

  @Setup(Level.Iteration)
  def setup(params: BenchmarkParams, eParam: EngineParam[S], work: Workload, size: Size, step: Step) = {
    engine = eParam.engine
    val threads = params.getThreads
    implicit val e = engine
    if (e == Engines.unmanaged) { isManual = true }
    sources = Range(0, threads).map(_ => Var(0)).toArray
    results = sources.map { source =>
      var cur: Signal[Int, S] = source
      for (x <- Range(0, size.size)) {cur = cur.map(1.+)}
      cur.map { x => {work.consume(); x} }
    }

    dynamics = results.zipWithIndex.map { case (r, i) =>
      e.Signal.dynamic {
        val v = r()
        val idx = i + (if (step.test(v)) 2 else 1)
        results(idx % threads)()
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
class Stacks[S <: Struct] {

  @Benchmark
  def run(state: StackState[S], step: Step, params: ThreadParams) = {
    if (state.isManual) state.synchronized {
      implicit val engine = state.engine
      val index = params.getThreadIndex % params.getThreadCount
      state.sources(index).set(step.run())
      state.dynamics(index).readValueOnce
    }
    else {
      implicit val engine = state.engine
      val index = params.getThreadIndex % params.getThreadCount
      state.sources(index).set(step.run())
      state.dynamics(index).readValueOnce
    }
  }


}
