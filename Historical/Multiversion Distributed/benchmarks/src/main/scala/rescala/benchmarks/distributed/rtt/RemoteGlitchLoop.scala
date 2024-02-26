package reactives.benchmarks.distributed.rtt

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
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
class RemoteGlitchLoop {
  var sourceEngine: FullMVEngine  = _
  var source: Var[Int]            = _
  var remoteEngine: FullMVEngine  = _
  var sourceOnRemote: Signal[Int] = _
  var remoteNode: Signal[Int]     = _
  var remoteOnSource: Signal[Int] = _
  var sourceMerge: Signal[Int]    = _
  var glitchCounter: Int          = _

  @Param(Array("50"))
  var msDelay: Int = _

  @Setup(Level.Iteration)
  def setup(): Unit = {
    FakeDelayer.enable()

    sourceEngine = new FullMVEngine(10.seconds, "src")
    source = {
      val e = sourceEngine
      import e._
      Var(0)
    }

    remoteEngine = new FullMVEngine(10.seconds, "remote")
    sourceOnRemote = ReactiveLocalClone(source, remoteEngine, msDelay.millis)
    remoteNode = {
      val e = remoteEngine
      import e._
      sourceOnRemote.map(_ + 1)
    }

    remoteOnSource = ReactiveLocalClone(remoteNode, sourceEngine, msDelay.millis)

    glitchCounter = 0
    sourceMerge = {
      val e = sourceEngine
      import e._
      val src = source
      val rmt = remoteOnSource
      Signal {
        val res = src() + rmt()
        if (res % 2 == 0) glitchCounter += 1
        res
      }
    }
  }

  @TearDown(Level.Iteration)
  def teardown(): Unit = FakeDelayer.shutdown()

  @Benchmark
  def run(): Unit = {
    val newValue = sourceEngine.forceNewTransaction(source) { t =>
      val newValue = t.now(source) + 1
      source.admit(newValue)(t)
      newValue
    }
    if (glitchCounter > 0) throw new IllegalStateException(s"a glitch occurred before or during update $newValue!")
  }
}
