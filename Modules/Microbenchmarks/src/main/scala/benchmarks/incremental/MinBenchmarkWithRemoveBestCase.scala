package benchmarks.incremental
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import rescala.extra.incremental.IncrementalApi.{_}

import java.util.concurrent.TimeUnit

/** @author gerizuna
  * @since 10.10.19
  */

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(3)
@State(Scope.Thread)
class MinBenchmarkWithRemoveBestCase {

  @Param(Array("1", "5", "10", "50", "100", "500", "1000", "5000", "10000"))
  var arg: Int = _

  var removeEvent: Evt[Int] = _
  var minOfSeq: Signal[Int] = _

  var reactSeq: SeqSource[Int]           = _
  var reactMinOfSeq: Signal[Option[Int]] = _

  @Setup(Level.Invocation)
  def prepare(): Unit = {
    removeEvent = Evt[Int]()
    val seq = removeEvent.fold((1 to arg).toList)((s, x) => {
      s diff Seq(x)
    })
    minOfSeq = Signal {
      seq.value.min
    }

    reactSeq = SeqSource.empty[Int]
    reactMinOfSeq = reactSeq.min
    seq.now.reverse.foreach(x => reactSeq.add(x))

  }

  @Benchmark
  def testSeq(blackHole: Blackhole): Unit = {

    removeEvent.fire(1)

    blackHole.consume(removeEvent)
    blackHole.consume(minOfSeq)
  }

  @Benchmark
  def testReactSeq(blackHole: Blackhole): Unit = {

    reactSeq.remove(1)

    blackHole.consume(reactSeq)
    blackHole.consume(reactMinOfSeq)
  }

}
