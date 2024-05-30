package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.{BenchmarkParams, Blackhole}

import java.util.concurrent.{CountDownLatch, TimeUnit}

@State(Scope.Benchmark)
class BusyThreads {
  @Param(Array("false"))
  var runBusyThreads: Boolean = scala.compiletime.uninitialized

  @volatile var running: Boolean = false
  var threads: Array[Thread]     = scala.compiletime.uninitialized
  @Setup(Level.Iteration)
  def bootBusyThreads(params: BenchmarkParams): Unit = {
    if runBusyThreads then {
      running = true
      val numProcs   = Runtime.getRuntime.availableProcessors()
      val idleProcs  = numProcs - params.getThreads
      val startLatch = new CountDownLatch(idleProcs)
      threads = Array.tabulate(idleProcs) { i =>
        val t = new Thread(s"busy-idler-$i") {
          override def run(): Unit = {
            startLatch.countDown()
            while running do Blackhole.consumeCPU(1000L)
          }
        }
        t.start()
        t
      }
      println(s"starting $idleProcs busy threads...")
      if !startLatch.await(1000, TimeUnit.MILLISECONDS) then {
        println(startLatch.getCount.toString + " busy threads failed to start")
      }
    }
  }
  @TearDown(Level.Iteration)
  def stopBusyThreads(): Unit = {
    if runBusyThreads then {
      println("stopping busy threads...")
      val timeout = System.currentTimeMillis() + 1000
      running = false
      for t <- threads do {
        t.join(timeout - System.currentTimeMillis())
        if t.isAlive then println(t.getName + " did not terminate!")
      }
    }
  }
}
