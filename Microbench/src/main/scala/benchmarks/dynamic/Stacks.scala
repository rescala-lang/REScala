package benchmarks.dynamic

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import benchmarks.{Size, EngineParam, Workload}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.{BenchmarkParams, ThreadParams}
import rescala._
import rescala.graph.Spores
import rescala.turns.{Engine, Ticket, Turn}

import scala.collection.immutable.Range


/**
 * creates a chain per thread, and connects the result dynamically with the chain of another
 * sources -> chain … chain -> result <- dynamically chain of other source
 */
@State(Scope.Benchmark)
class StackState[S <: Spores] {

  var sources: Array[Var[Int, S]] = _
  var results: Array[Signal[Int, S]] = _
  var dynamics: Array[Signal[Int, S]] = _
  var engine: Engine[S, Turn[S]] = _

  @Setup(Level.Iteration)
  def setup(params: BenchmarkParams, engine: EngineParam[S], work: Workload, size: Size) = {
    this.engine = engine.engine
    val threads = params.getThreads
    implicit val e = this.engine
    sources = Range(0, threads).map(_ => Var(0)).toArray
    results = sources.map { source =>
      var cur: Signal[Int, S] = source
      for (x <- Range(0, size.size)) {cur = cur.map(1.+)}
      cur.map { x => work.consume(); x }
    }
    dynamics = results.zipWithIndex.map { case (r, i) =>
      Signals.dynamic(r) { t =>
        val v = r(t)
        val idx = i + (if (v % 17 == 0) v else 1)
        sources((i + threads) % threads)(t)
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
class Stacks[S <: Spores] {

  @Benchmark
  def run(state: StackState[S], params: ThreadParams) = {
    val index = params.getThreadIndex % params.getThreadCount
    state.sources(index).set(state.input.incrementAndGet())(state.engine)
    state.dynamics(index).now(Ticket.dynamic(state.engine))
  }


}
