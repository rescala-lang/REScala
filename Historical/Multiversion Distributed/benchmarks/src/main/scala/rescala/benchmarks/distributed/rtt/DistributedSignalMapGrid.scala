package reactives.benchmarks.distributed.rtt

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import reactives.fullmv.DistributedFullMVApi.{CreationTicket, ReactiveLocalClone, FullMVEngine, Signal, Var}
import reactives.core.ReInfo
import reactives.fullmv.mirrors.localcloning.FakeDelayer

import scala.concurrent.duration._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 5000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 7, time = 5000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class DistributedSignalMapGrid {
  var sourceEngine: FullMVEngine                             = _
  var source: Var[Int]                                       = _
  var nodes: Seq[Seq[(FullMVEngine, Seq[Seq[Signal[Int]]])]] = _
  @Param(Array("50"))
  var msDelay: Int = _
  @Param(Array("1", "2", "3", "5"))
  var depthHosts: Int = _
  @Param(Array("2"))
  var widthHosts: Int = _
  @Param(Array("0"))
  var depthNodesPerHost: Int = _
  @Param(Array("2"))
  var widthNodesPerHost: Int = _
  @Param(Array("0"))
  var work: Int = _

  @Setup(Level.Iteration)
  def setup(): Unit = {
    FakeDelayer.enable()

    sourceEngine = new FullMVEngine(10.seconds, "src")
    source = {
      val e = sourceEngine
      import e._
      Var(0)
    }

    var result: Seq[(FullMVEngine, Seq[Signal[Int]])] =
      Seq.fill(widthHosts)(sourceEngine -> Seq.fill(widthNodesPerHost)(source))
    nodes = for (dh <- 1 to depthHosts) yield {
      val r = for (wh <- 1 to widthHosts) yield {
        val host = new FullMVEngine(10.seconds, s"host-$dh-$wh")
        var res2 = for (wn <- 1 to widthNodesPerHost) yield {
          val from = result(wh - 1)._2(wn - 1)
          val name = s"clone-$dh-$wh-0-$wn"
//          println(s"cloning $name from $from")
          ReInfo.named(name) { implicit ! =>
            ReactiveLocalClone(from, host, msDelay.millis)
          }
        }
        host -> (res2 +: (for (dn <- 1 to depthNodesPerHost) yield {
          res2 = for (wn <- 1 to widthNodesPerHost) yield {
            val from = res2(wn - 1)
            val name = s"map-$dh-$wh-$dn-$wn"
//            println(s"transforming $name from $from")
            ReInfo.named(name) { implicit ! =>
              from.map { v =>
                Blackhole.consumeCPU(work); v + 1
              }(host)
            }
          }
          res2
        })) -> (host -> res2)
      }
      result = r.map(_._2)
      r.map(_._1)
    }
  }

  @TearDown(Level.Iteration)
  def teardown(): Unit = FakeDelayer.shutdown()

  @Benchmark
  def run(): Unit = {
    source.transform(_ + 1)(sourceEngine, implicitly)
  }
}
