package benchmarks.simple

import java.util.concurrent.TimeUnit

import benchmarks._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.{BenchmarkParams, ThreadParams}
import rescala.interface.RescalaInterface

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class SingleNodeContentionProfiling extends BusyThreads {
  var engine: RescalaInterface = _
  final lazy val stableEngine  = engine
  import stableEngine._

  var sources: Array[Var[Int]] = _
  var node: Signal[Unit]       = _

  @Setup(Level.Iteration)
  def setup(params: BenchmarkParams, step: Step, engineParam: EngineParam, work: Workload) = {
    engine = engineParam.engine
    sources = Array.fill(params.getThreads)(Var(step.run()))
    node = Signals.static(sources.toSeq: _*)(_ => work.consume())
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
