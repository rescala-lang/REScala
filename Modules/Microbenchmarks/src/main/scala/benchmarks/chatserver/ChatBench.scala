package benchmarks.chatserver

import benchmarks.{EngineParam, Size, Workload}
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.{BenchmarkParams, ThreadParams}
import reactives.operator.Interface

import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.{Lock, ReentrantLock}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class ChatBench {

  @Benchmark
  def chat(benchState: BenchState, threadParams: ThreadParams) = {
    import benchState.stableEngine._
    if (scheduler != reactives.interfaces.unmanaged.scheduler) {
      benchState.clients(threadParams.getThreadIndex).fire("hello")
    } else {
      val ti    = threadParams.getThreadIndex
      val locks = benchState.locks
      val room1 = math.min(ti % locks.length, (ti + locks.length / 2) % locks.length)
      val room2 = math.max(ti % locks.length, (ti + locks.length / 2) % locks.length)
      locks(room1).lock()
      locks(room2).lock()
      try {
        benchState.clients(threadParams.getThreadIndex).fire("hello")
      } finally {
        locks(room2).unlock()
        locks(room1).unlock()
      }
    }
  }

}

@State(Scope.Benchmark)
class BenchState {

  var engine: Interface       = scala.compiletime.uninitialized
  final lazy val stableEngine = engine
  import stableEngine._

  var cs: ChatServer[stableEngine.type] = scala.compiletime.uninitialized
  var clients: Array[Evt[String]]       = scala.compiletime.uninitialized
  var locks: Array[Lock]                = null

  @Setup
  def setup(params: BenchmarkParams, engineParam: EngineParam, size: Size, work: Workload) = {
    engine = engineParam.engine

    val threads = params.getThreads

    cs = new ChatServer[stableEngine.type]()(stableEngine)
    Range(0, size.size).foreach(cs.create)

    clients = Array.fill(threads)(Evt[String]())
    for ((client, i) <- clients.zipWithIndex) {
      val room1 = i                   % size.size
      val room2 = (i + size.size / 2) % size.size
      cs.join(client, room1)
      cs.join(client, room2)
      cs.histories.get(room1).observe(v => work.consume())
      cs.histories.get(room2).observe(v => work.consume())
    }

    if (engine == reactives.interfaces.unmanaged) {
      locks = Array.fill(size.size)(new ReentrantLock())
    }

  }

}
