package benchmarks.simple

import benchmarks.*
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.{BenchmarkParams, ThreadParams}


import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class SingleNodeContentionProfiling extends BusyThreads {
  var engine: reactives.default.type     = scala.compiletime.uninitialized
  final lazy val stableEngine = engine
  import stableEngine.*

  var sources: Array[Var[Int]] = scala.compiletime.uninitialized
  var node: Signal[Unit]       = scala.compiletime.uninitialized

  @Setup(Level.Iteration)
  def setup(params: BenchmarkParams, step: Step, engineParam: EngineParam, work: Workload) = {
    engine = engineParam.engine
    sources = Array.fill(params.getThreads)(Var(step.run()))
    node = Signal.static(sources.toSeq*)(_ => work.consume())
  }

  @Benchmark
  def run(threadParams: ThreadParams, step: Step): Unit = sources(threadParams.getThreadIndex).set(step.run())

//  @TearDown(Level.Trial) def printStats(p: BenchmarkParams): Unit = {
//    println()
//    println(s"Threads\tPhase\tkind\tRestartResult\tCount")
//    printMap(p, "Framing", "spin switch", FullMVTurn.spinSwitchStatsFraming)
//    printMap(p, "Framing", "spin restart", FullMVTurn.spinRestartStatsFraming)
//    printMap(p, "Framing", "park switch", FullMVTurn.parkSwitchStatsFraming)
//    printMap(p, "Framing", "park restart", FullMVTurn.parkRestartStatsFraming)
//    printMap(p, "Executing", "spin switch", FullMVTurn.spinSwitchStatsExecuting)
//    printMap(p, "Executing", "spin restart", FullMVTurn.spinRestartStatsExecuting)
//    printMap(p, "Executing", "park switch", FullMVTurn.parkSwitchStatsExecuting)
//    printMap(p, "Executing", "park restart", FullMVTurn.parkRestartStatsExecuting)
//  }
//
//  private def printMap(p: BenchmarkParams, phase: String, kind: String, map: util.HashMap[_, _]) = {
//    val it = map.entrySet().iterator()
//    while (it.hasNext) {
//      val entry = it.next()
//      println(s"${p.getThreads}\t$phase\t$kind\t${entry.getKey}\t${entry.getValue}")
//    }
//    map.clear()
//  }
}
