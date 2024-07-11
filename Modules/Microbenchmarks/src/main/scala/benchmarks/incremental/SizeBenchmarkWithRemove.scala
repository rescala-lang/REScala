package benchmarks.incremental
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import reactives.default.*
import reactives.extra.incremental.*

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
class SizeBenchmarkWithRemove {

  @Param(Array("1", "5", "10", "50", "100", "500", "1000", "5000", "10000"))
  var arg: Int = scala.compiletime.uninitialized

  var removeEvent: Evt[Int]  = scala.compiletime.uninitialized
  var sizeOfSeq: Signal[Int] = scala.compiletime.uninitialized

  var reactSeq: IncSeq[Int]       = scala.compiletime.uninitialized
  var sizeOfReactSeq: Signal[Int] = scala.compiletime.uninitialized

  @Setup(Level.Invocation)
  def prepare(): Unit = {
    removeEvent = Evt[Int]()
    val seq = removeEvent.fold((1 to arg).toList)((s, x) => {
      s `diff` Seq(x)
    })
    sizeOfSeq = Signal {
      seq.value.size
    }

    reactSeq = IncSeq.empty[Int]
    sizeOfReactSeq = reactSeq.size
    seq.now.foreach(x => reactSeq.add(x))
  }

  @Benchmark
  def testSeq(blackHole: Blackhole): Unit = {
    removeEvent.fire(arg)
    blackHole.consume(removeEvent)
    blackHole.consume(sizeOfSeq)
  }

  @Benchmark
  def testReactSeq(blackHole: Blackhole): Unit = {
    reactSeq.remove(arg)
    blackHole.consume(reactSeq)
    blackHole.consume(sizeOfReactSeq)
  }
}
