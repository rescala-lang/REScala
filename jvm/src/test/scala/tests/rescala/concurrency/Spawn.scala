package tests.rescala.concurrency

import java.util.concurrent.CountDownLatch

object Spawn {
  def apply(f: => Unit, name: String = "unnamed spawned thread"): Thread = {
    val latch = new CountDownLatch(1)
    val t = new Thread(new Runnable {
      override def run(): Unit = {
        latch.countDown()
        f
      }
    }, name)
    t.start()
    latch.await()
    t
  }
}
