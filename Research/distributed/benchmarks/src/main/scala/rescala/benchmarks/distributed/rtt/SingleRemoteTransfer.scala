package rescala.benchmarks.distributed.rtt

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import rescala.core.REName
import rescala.fullmv.mirrors.localcloning.{FakeDelayer, ReactiveLocalClone}
import rescala.fullmv.{FullMVEngine, FullMVStruct}
import rescala.reactives.{Signal, Var}

import scala.concurrent.duration._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 5000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 7, time = 5000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class SingleRemoteTransfer {
  var sourceEngine: FullMVEngine = _
  var source: Var[Int, FullMVStruct] = _
  var nodes: Seq[(FullMVEngine, Signal[Int, FullMVStruct])] = _
  @Param(Array("50"))
  var msDelay: Int = _
  @Param(Array("1", "2", "3", "5"))
  var depth: Int = _

  @Setup(Level.Iteration)
  def setup(): Unit = {
    FakeDelayer.enable()

    sourceEngine = new FullMVEngine(10.seconds,"src")
    source = {
      val e = sourceEngine
      import e._
      sourceEngine.Var(0)
    }

    var result: (FullMVEngine, Signal[Int, FullMVStruct]) = sourceEngine -> source
    nodes = for (i <- 1 to depth) yield {
      val host = new FullMVEngine(10.seconds, s"host-$i")
      result = host -> REName.named(s"clone-$i") { implicit ! =>
        ReactiveLocalClone(result._2, host, msDelay.millis)
      }
      result
    }
  }

  @TearDown(Level.Iteration)
  def teardown(): Unit = FakeDelayer.shutdown()

  @Benchmark
  def run(): Unit = {
    source.transform(_ + 1)(sourceEngine)
  }
}
