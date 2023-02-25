package benchmarks.incremental
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import rescala.extra.incremental.IncrementalApi.{State => _, _}

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
class MapBenchmarkWithInsert {

  @Param(Array("1", "5", "10", "50", "100", "500", "1000", "5000", "10000"))
  var arg: Int = _

  var addEvent: Evt[Int]          = _
  var mappedSeq: Signal[Seq[Int]] = _

  var reactSeq: SeqSource[Int]              = _
  var reactMappedSeq: ReactiveDeltaSeq[Int] = _

  @Setup(Level.Invocation)
  def prepare(): Unit = {
    addEvent = Evt[Int]()
    val seq = addEvent.fold(Seq.range(1, arg))((s, x) => {
      s :+ x
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
    addEvent.fire(arg)
    blackHole.consume(addEvent)
    blackHole.consume(mappedSeq)
  }

  @Benchmark
  def testReactSeq(blackHole: Blackhole): Unit = {
    reactSeq.add(arg)
    blackHole.consume(reactSeq)
    blackHole.consume(reactMappedSeq)
  }

}
