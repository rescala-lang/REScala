package benchmarks.simple

import benchmarks._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.ThreadParams
import rescala.interface.RescalaInterface

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class LowContentionSerialOrder extends BusyThreads {
  var engine: RescalaInterface = _
  final lazy val stableEngine  = engine
  import stableEngine._
  var sources: Array[Var[Int]]        = _
  var grid: Array[Array[Signal[Int]]] = _
  @Param(Array("16"))
  var size: Int = _

  @Setup(Level.Iteration)
  def setup(engineParam: EngineParam, work: Workload) = {
    engine = engineParam.engine
    sources = Array.fill(size)(Var(0))
    grid = Array.tabulate(size - 1) { t =>
      val a = size - 1 - t
      Array.tabulate(a) { b =>
        Signal.lift(sources(a), sources(b)) { (va, vb) =>
          work.consume(); va + vb
        }
      }
    }
  }

  @Benchmark
  def run(threadParams: ThreadParams): Unit = sources(threadParams.getThreadIndex).transform(_ + 1)

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
