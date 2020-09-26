package benchmarks.chatserver

import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.{Lock, ReentrantLock}

import benchmarks.{EngineParam, Size, Workload}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.{BenchmarkParams, ThreadParams}
import rescala.Schedulers
import rescala.core.{Scheduler, Struct}
import rescala.interface.RescalaInterface
import rescala.reactives.Evt

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class ChatBench[S <: Struct] {

  @Benchmark
  def chat(benchState: BenchState[S], threadParams: ThreadParams) = {
    implicit def scheduler: Scheduler[S] = benchState.engine.scheduler
    if (benchState.engine.scheduler != Schedulers.unmanaged) {
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
class BenchState[S <: Struct] {

  var cs: ChatServer[S]              = _
  var clients: Array[Evt[String, S]] = _
  var locks: Array[Lock]             = null
  var engine: RescalaInterface[S]    = _

  @Setup
  def setup(params: BenchmarkParams, work: Workload, engineParam: EngineParam[S], size: Size) = {
    engine = engineParam.engine
    implicit def scheduler: Scheduler[S] = engine.scheduler

    val threads = params.getThreads

    cs = new ChatServer[S]()(engine)
    Range(0, size.size).foreach(cs.create)

    clients = Array.fill(threads)(engine.Evt[String]())
    for ((client, i) <- clients.zipWithIndex) {
      val room1 = i                   % size.size
      val room2 = (i + size.size / 2) % size.size
      cs.join(client, room1)
      cs.join(client, room2)
      cs.histories.get(room1).observe(v => work.consume())
      cs.histories.get(room2).observe(v => work.consume())
    }

    if (engine.scheduler == Schedulers.unmanaged) {
      locks = Array.fill(size.size)(new ReentrantLock())
    }

  }

}
