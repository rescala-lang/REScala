package tests.rescala.concurrency

import java.util.concurrent._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}

class Spawn[T](val future: Future[T]) {
  def join(milliseconds: Long): T = {
    Await.ready(future, milliseconds.millis)
    future.value.get match {
      case Success(v) => v
      case Failure(ex) => throw new Spawn.SpawnedThreadFailedException(ex)
    }
  }
}
object Spawn {
  class SpawnedThreadFailedException(ex: Throwable) extends Exception(ex)
  val threadpool = ExecutionContext.fromExecutor(new ThreadPoolExecutor(0, Integer.MAX_VALUE, 1L, TimeUnit.SECONDS, new SynchronousQueue[Runnable]()))

  def apply[T](f: => T, name: String = "unnamed spawned thread"): Spawn[T] = {
    object lock
    var started = false
    val fut = Future {
      lock.synchronized{
        started = true
        lock.notifyAll()
      }
      f
    }(threadpool)
    lock.synchronized {
      while(!started) lock.wait()
    }
    new Spawn(fut)
  }
}
