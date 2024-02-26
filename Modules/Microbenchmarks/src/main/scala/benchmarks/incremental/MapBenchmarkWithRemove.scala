package benchmarks.incremental
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import reactives.extra.incremental.IncrementalApi._

import java.util.concurrent.TimeUnit
import scala.util.Random

/** @author gerizuna
  * @since 10.10.19
  */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(3)
@State(Scope.Thread)
class MapBenchmarkWithRemove {

  @Param(Array("1", "5", "10", "50", "100", "500", "1000", "5000", "10000"))
  var arg: Int = scala.compiletime.uninitialized

  var removeEvent: Evt[Int]       = scala.compiletime.uninitialized
  var mappedSeq: Signal[Seq[Int]] = scala.compiletime.uninitialized

  var reactSeq: SeqSource[Int]              = scala.compiletime.uninitialized
  var reactMappedSeq: ReactiveDeltaSeq[Int] = scala.compiletime.uninitialized

  @Setup(Level.Invocation)
  def prepare(): Unit = {
    removeEvent = Evt[Int]()
    val seq = removeEvent.fold((1 to arg).toList)((s, x) => {
      s diff Seq(x)
    })
    mappedSeq = Signal {
      seq.value.map(x => {
        x * x
      })
    }

    reactSeq = SeqSource.empty[Int]
    reactMappedSeq = reactSeq.map(x => x * x)
    seq.now.foreach(x => reactSeq.add(x))
  }

  @Benchmark
  def testSeq(blackHole: Blackhole): Unit = {
    var toRemove = Random.nextInt(arg)
    if (toRemove < 1)
      toRemove = 1

    removeEvent.fire(toRemove)
    blackHole.consume(removeEvent)
    blackHole.consume(mappedSeq)
  }

  @Benchmark
  def testReactSeq(blackHole: Blackhole): Unit = {
    var toRemove = Random.nextInt(arg)
    if (toRemove < 1)
      toRemove = 1

    reactSeq.remove(toRemove)
    blackHole.consume(reactSeq)
    blackHole.consume(reactMappedSeq)
  }

}
