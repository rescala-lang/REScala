package rescala.benchmarks.distributed.rtt

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import rescala.core.REName
import rescala.fullmv.mirrors.localcloning.ReactiveLocalClone
import rescala.fullmv.{FullMVEngine, FullMVStruct}
import rescala.reactives.{Signal, Var}

import scala.concurrent.duration._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 10, time = 2000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 20, time = 2000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class SingleRemoteTransfer {
  var sourceEngine: FullMVEngine = _
  var source: Var[Int, FullMVStruct] = _
  var nodes: Seq[(FullMVEngine, Signal[Int, FullMVStruct])] = _
  @Param(Array("100"))
  var msDelay: Int = _
  @Param(Array("1", "2"))
  var depth: Int = _

  @Setup(Level.Iteration)
  def setup() = {
    sourceEngine = new FullMVEngine(10.seconds,"src")
    source = {
      val e = sourceEngine
      import e._
      sourceEngine.Var(0)
    }
    
    var result: (FullMVEngine, Signal[Int, FullMVStruct]) = sourceEngine -> source
    for (i <- 1 to depth) {
      val host = new FullMVEngine(10.seconds, s"host-$i")
      result = host -> REName.named(s"clone-$i") { implicit ! =>
        ReactiveLocalClone(result._2, host, 25.millis)
      }
    }
  }

  @Benchmark
  def run(): Unit = {
    source.transform(_ + 1)(sourceEngine)
  }
}
