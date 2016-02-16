package benchmarks.chatserver

import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock

import benchmarks.{Size, EngineParam, Workload}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.{Blackhole, ThreadParams, BenchmarkParams}
import rescala.reactives.{Evt, Event}
import rescala.graph.Struct


@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class ChatBench[S <: Struct] {

  @Benchmark
  def chat(benchState: BenchState[S], threadParams: ThreadParams, engineParam: EngineParam[S]) = {
    benchState.clients(threadParams.getThreadIndex)("hello")(engineParam.engine)
  }

}


@State(Scope.Benchmark)
class BenchState[S <: Struct] {


  var cs: ChatServer[S] = _
  var clients: Array[Evt[String, S]] = _

  @Setup
  def setup(params: BenchmarkParams, work: Workload, engineParam: EngineParam[S], size: Size) = {
    implicit val engine = engineParam.engine
    import engine._

    val threads = params.getThreads

    cs = new ChatServer[S]()
    Range(0, size.size).foreach(cs.create)

    clients = Array.fill(threads)(engine.Evt[String]())
    for ((client, i) <- clients.zipWithIndex) {
      val room1 = i % size.size
      val room2 = (i + size.size / 2) % size.size
      cs.join(client, room1)
      cs.join(client, room2)
      cs.histories.get(room1).observe(v => work.consume())
      cs.histories.get(room2).observe(v => work.consume())
    }

  }


}