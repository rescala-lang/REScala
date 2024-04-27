package tests.rescala.testtools

import java.util.concurrent.CountDownLatch
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future, Promise, TimeoutException}
import scala.util.{Failure, Try}

class Spawn[T] private (future: Future[T]) {
  def await(millis: Long): T = {
    Await.result(future, millis.millisecond)
  }
  def awaitTry(millis: Long): Try[T] = {
    try {
      Await.ready(future, millis.milliseconds)
      future.value.get
    } catch {
      case te: TimeoutException => Failure(te)
    }
  }
}
object Spawn {
  def apply[T](f: => T, desiredName: Option[String] = None): Spawn[T] = {
    val promise = Promise[T]()
    val latch   = new CountDownLatch(1)
    val runnable = new Runnable {
      override def run(): Unit = {
        latch.countDown()
        promise.complete(Try(f))
        ()
      }
    }
    val t = desiredName.fold(new Thread(runnable))(new Thread(runnable, _))
    t.start()
    latch.await()
    new Spawn[T](promise.future)
  }
}
