package tests.rescala.testtools

import java.util.concurrent._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class Spawn[T](val threadName: String, val future: Future[T]) {
  def join(milliseconds: Long): T = {
    Await.ready(future, milliseconds.millis).value.get match {
      case Success(v) => v
      case Failure(ex) => throw new Spawn.SpawnedThreadFailedException(threadName, ex)
    }
  }
  def await(milliseconds: Long): Try[T] = {
    Try { Await.result(future, milliseconds.millis) }
  }
}
object Spawn {
  class SpawnedThreadFailedException(threadName: String, ex: Throwable) extends Exception("Thread "+threadName+" failed.", ex)
  val threadpool = ExecutionContext.fromExecutor(new ThreadPoolExecutor(0, Integer.MAX_VALUE, 1L, TimeUnit.SECONDS, new SynchronousQueue[Runnable]()))

  def apply[T](f: => T, desiredName: Option[String] = None): Spawn[T] = {
    object lock
    var actualName: Option[String] = None
    val fut = Future {
      val oldThreadName = Thread.currentThread().getName()
      if(desiredName.isDefined) Thread.currentThread().setName(desiredName.get)
      try {
        lock.synchronized {
          actualName = Some(Thread.currentThread().getName)
          lock.notifyAll()
        }
        f
      } finally {
        Thread.currentThread().setName(oldThreadName)
      }
    }(threadpool)
    lock.synchronized {
      while(actualName.isEmpty) lock.wait()
    }
    new Spawn(actualName.get, fut)
  }
}
