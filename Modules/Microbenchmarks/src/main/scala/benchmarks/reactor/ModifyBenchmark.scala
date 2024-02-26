package benchmarks.reactor

import benchmarks.EngineParam
import org.openjdk.jmh.annotations.*
import reactives.extra.reactor.{Reactor, S}
import reactives.operator.Interface

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(5)
@Threads(1)
@State(Scope.Thread)
class ModifyBenchmark {
  var engine: Interface       = scala.compiletime.uninitialized
  final lazy val stableEngine = engine
  import stableEngine._

  var reactor: Reactor[Int] = scala.compiletime.uninitialized
  var trigger: Evt[Unit]    = scala.compiletime.uninitialized

  @Setup
  def setup(engineParam: EngineParam) = {
    engine = engineParam.engine
    trigger = Evt[Unit]()
    reactor = Reactor.loop(0) {
      S.next(trigger) {
        S.modify(v => v + 1)
      }
    }
  }

  @Benchmark
  def run(): Unit = trigger.fire()
}
