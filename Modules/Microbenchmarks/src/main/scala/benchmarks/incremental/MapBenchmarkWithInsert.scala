package benchmarks.incremental
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import reactives.extra.incremental.IncrementalApi.*

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
  var arg: Int = scala.compiletime.uninitialized

  var addEvent: Evt[Int]          = scala.compiletime.uninitialized
  var mappedSeq: Signal[Seq[Int]] = scala.compiletime.uninitialized

  var reactSeq: SeqSource[Int]              = scala.compiletime.uninitialized
  var reactMappedSeq: ReactiveDeltaSeq[Int] = scala.compiletime.uninitialized

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
