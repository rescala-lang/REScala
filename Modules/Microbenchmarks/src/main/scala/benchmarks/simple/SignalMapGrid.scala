package benchmarks.simple

import benchmarks.*
import org.openjdk.jmh.annotations.*
import reactives.core.{CreationTicket, ReInfo}
import reactives.operator.Interface

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class SignalMapGrid extends BusyThreads {
  var engine: Interface       = scala.compiletime.uninitialized
  final lazy val stableEngine = engine
  import stableEngine.*

  var source: Var[Int]        = scala.compiletime.uninitialized
  var leafs: Seq[Signal[Int]] = scala.compiletime.uninitialized
  @Param(Array("1", "4", "16"))
  var width: Int = scala.compiletime.uninitialized
  @Param(Array("1", "4", "16"))
  var depth: Int = scala.compiletime.uninitialized

  @Setup(Level.Iteration)
  def setup(engineParam: EngineParam, work: Workload) = {
    engine = engineParam.engine
    source = Var(0)
    leafs =
      for w <- 1 to width yield {
        var result: Signal[Int] = source
        for d <- 1 to depth do {
          result = {
            result.map { v =>
              work.consume(); v + 1
            }(using CreationTicket.fromName(s"map-$w-$d"))
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
