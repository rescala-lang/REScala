package benchmarks.simple

import java.util.concurrent.TimeUnit

import benchmarks._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.core.{Scheduler, REName, Struct}
import rescala.reactives.{Signal, Var}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class SignalMapGrid[S <: Struct] extends BusyThreads {
  implicit var engine: Scheduler[S] = _
  var source: Var[Int, S] = _
  var leafs: Seq[Signal[Int, S]] = _
  @Param(Array("1", "4", "16"))
  var width: Int = _
  @Param(Array("1", "4", "16"))
  var depth: Int = _

  @Setup(Level.Iteration)
  def setup(params: BenchmarkParams, engineParam: EngineParam[S], work: Workload) = {
    engine = engineParam.engine
    source = Var(0)
    leafs = for (w <- 1 to width) yield {
      var result: Signal[Int, S] = source
      for (d <- 1 to depth) {
        result = REName.named(s"map-$w-$d") { implicit ! =>
          result.map { v => work.consume(); v + 1 }
        }
      }
      result
    }
  }

  @Benchmark
  def run(): Unit = source.transform(_ + 1)

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
