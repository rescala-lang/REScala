package benchmarks.simple

import benchmarks.{EngineParam, Size, Step, Workload}
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.{BenchmarkParams, ThreadParams}

import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.{Lock, ReentrantLock}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(4)
@State(Scope.Benchmark)
class MultiReverseFan {

  var engine: reactives.default.type = scala.compiletime.uninitialized
  final lazy val stableEngine        = engine
  import stableEngine.*

  var sources: Array[Var[Int]]    = scala.compiletime.uninitialized
  var results: Array[Signal[Int]] = scala.compiletime.uninitialized
  var locks: Array[Lock] | Null   = null
  var groupSize: Int              = scala.compiletime.uninitialized

  @Setup
  def setup(params: BenchmarkParams, size: Size, step: Step, engineParam: EngineParam, work: Workload) = {
    engine = engineParam.engine
    val threads = params.getThreads

    sources = Array.fill(threads)(Var(step.get()))
    groupSize = if threads > size.size then threads / size.size else 1

    val intermediate = sources.map(_.map { v => { work.consume(); v + 1 } }).grouped(groupSize)
    results = intermediate.map { sigs =>
      Signal.lift(sigs.toSeq) { values =>
        work.consumeSecondary(); values.sum
      }
    }.toArray

    if engineParam.engineName == "unmanaged" then locks = Array.fill(threads / groupSize)(new ReentrantLock())

  }

  @Benchmark
  def run(step: Step, params: ThreadParams): Unit = {
    val index = params.getThreadIndex
    if locks eq null then sources(index).set(step.run())
    else {
      locks.nn(index / groupSize).lock()
      try {
        sources(index).set(step.run())
      } finally locks.nn(index / groupSize).unlock()
    }
  }
}
