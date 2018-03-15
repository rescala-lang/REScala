package tests.rescala.testtools

import java.util.concurrent._

object Spawn {
  def apply[T](f: => T, desiredName: Option[String] = None): Thread = {
    val latch = new CountDownLatch(1)
    val runnable = new Runnable {
      override def run(): Unit = {
        latch.countDown()
        f
      }
    }
    val t = desiredName.fold(new Thread(runnable))(new Thread(runnable, _))
    t.start()
    latch.await()
    t
  }
}
