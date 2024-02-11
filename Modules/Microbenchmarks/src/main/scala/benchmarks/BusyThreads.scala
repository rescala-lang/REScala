package benchmarks

import java.util.concurrent.{CountDownLatch, TimeUnit}

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.{BenchmarkParams, Blackhole}

@State(Scope.Benchmark)
class BusyThreads {
  @Param(Array("false"))
  var runBusyThreads: Boolean = scala.compiletime.uninitialized

  @volatile var running: Boolean = false
  var threads: Array[Thread]     = scala.compiletime.uninitialized
  @Setup(Level.Iteration)
  def bootBusyThreads(params: BenchmarkParams): Unit = {
    if (runBusyThreads) {
      running = true
      val numProcs   = Runtime.getRuntime.availableProcessors()
      val idleProcs  = numProcs - params.getThreads
      val startLatch = new CountDownLatch(idleProcs)
      threads = Array.tabulate(idleProcs) { i =>
        val t = new Thread(s"busy-idler-$i") {
          override def run(): Unit = {
            startLatch.countDown()
            while (running) Blackhole.consumeCPU(1000L)
          }
        }
        t.start()
        t
      }
      println(s"starting $idleProcs busy threads...")
      if (!startLatch.await(1000, TimeUnit.MILLISECONDS)) {
        println(startLatch.getCount.toString + " busy threads failed to start")
      }
    }
  }
  @TearDown(Level.Iteration)
  def stopBusyThreads(): Unit = {
    if (runBusyThreads) {
      println("stopping busy threads...")
      val timeout = System.currentTimeMillis() + 1000
      running = false
      for (t <- threads) {
        t.join(timeout - System.currentTimeMillis())
        if (t.isAlive) println(t.getName + " did not terminate!")
      }
    }
  }
}
