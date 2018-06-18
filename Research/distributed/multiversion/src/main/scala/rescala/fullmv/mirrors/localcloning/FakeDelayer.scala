package rescala.fullmv.mirrors.localcloning

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ScheduledThreadPoolExecutor, TimeUnit}

import rescala.fullmv.FullMVEngine

import scala.concurrent.duration.Duration
import scala.concurrent.{Future, Promise}

object FakeDelayer {
  var executor: ScheduledThreadPoolExecutor = _

  def enable(): Unit = {
    if(executor != null) throw new IllegalStateException("Fake delay is already enabled, maybe someone forgot to .shutdown()?")
    executor = new ScheduledThreadPoolExecutor(1)
  }
  def shutdown(): Unit = {
    executor.shutdown()
    executor = null
  }

  val i = new AtomicInteger(0)
  def async(fakeDelay: Duration, op: => Unit): Unit = {
    if(fakeDelay == Duration.Zero) {
      op
    } else {
      if(FakeDelayer.executor == null) throw new IllegalStateException("Fake delay must be explicitly .enable()'d!")
      val id = i.incrementAndGet()
      println(s"[${System.currentTimeMillis()}] scheduling async $id: ${Thread.currentThread().getStackTrace()(2).toString}")
      FakeDelayer.executor.schedule(new Runnable() {
        override def run(): Unit = {
          println(s"[${System.currentTimeMillis()}] executing async $id")
          op
        }
      }, fakeDelay.toMillis, TimeUnit.MILLISECONDS)
    }
  }

  def future[V](fakeDelay: Duration, future: => Future[V]): Future[V] = {
    if(fakeDelay == Duration.Zero) {
      future
    } else {
      if(FakeDelayer.executor == null) throw new IllegalStateException("Fake delay must be explicitly .enable()'d!")
      val promise = Promise[V]
      val id = i.incrementAndGet()
      println(s"[${System.currentTimeMillis()}] scheduling request $id: ${Thread.currentThread().getStackTrace()(2).toString}")
      FakeDelayer.executor.schedule(new Runnable() {
        override def run(): Unit = {
          println(s"[${System.currentTimeMillis()}] executing request $id")
          future.onComplete { v =>
            println(s"[${System.currentTimeMillis()}] scheduling reply for $id: $v")
            FakeDelayer.executor.schedule(new Runnable() {
              override def run(): Unit = {
                println(s"[${System.currentTimeMillis()}] delivering reply for $id")
                promise.complete(v)
              }
            }, fakeDelay.toMillis, TimeUnit.MILLISECONDS)
          }(FullMVEngine.notWorthToMoveToTaskpool)
        }
      }, fakeDelay.toMillis, TimeUnit.MILLISECONDS)
      promise.future
    }
  }
}
