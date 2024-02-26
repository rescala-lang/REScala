package reactives.fullmv.mirrors.localcloning

import reactives.fullmv.FullMVUtil

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ScheduledThreadPoolExecutor, TimeUnit}
import reactives.fullmv.mirrors.Host

import scala.concurrent.duration.Duration
import scala.concurrent.{Future, Promise}

object FakeDelayer {
  val LOGGING: Boolean = false

  var executor: ScheduledThreadPoolExecutor = _

  def enable(): Unit = {
    if (executor != null)
      throw new IllegalStateException("Fake delay is already enabled, maybe someone forgot to .shutdown()?")
    executor = new ScheduledThreadPoolExecutor(1)
  }
  def shutdown(): Unit = {
    executor.shutdown()
    executor = null
  }

  val i = new AtomicInteger(0)
  def async(from: Host[_], to: Host[_], fakeDelay: Duration, op: => Unit): Unit = {
    if (fakeDelay == Duration.Zero) {
      op
    } else {
      if (FakeDelayer.executor == null) throw new IllegalStateException("Fake delay must be explicitly .enable()'d!")
      val id = i.incrementAndGet()
      if (LOGGING)
        println(
          s"[${System.currentTimeMillis()}] $from to $to async $id: ${Thread.currentThread().getStackTrace()(2).toString}"
        )
      FakeDelayer.executor.schedule(
        new Runnable() {
          override def run(): Unit = {
            if (LOGGING) println(s"[${System.currentTimeMillis()}] $to executing async $id")
            op
          }
        },
        fakeDelay.toMillis,
        TimeUnit.MILLISECONDS
      )
      ()
    }
  }

  def requestReply[V](from: Host[_], to: Host[_], fakeDelay: Duration, future: => Future[V]): Future[V] = {
    if (fakeDelay == Duration.Zero) {
      future
    } else {
      if (FakeDelayer.executor == null) throw new IllegalStateException("Fake delay must be explicitly .enable()'d!")
      val promise = Promise[V]()
      val id      = i.incrementAndGet()
      if (LOGGING)
        println(
          s"[${System.currentTimeMillis()}] $from to $to request $id: ${Thread.currentThread().getStackTrace()(2).toString}"
        )
      FakeDelayer.executor.schedule(
        new Runnable() {
          override def run(): Unit = {
            if (LOGGING) println(s"[${System.currentTimeMillis()}] $to executing request $id")
            future.onComplete { v =>
              if (LOGGING) println(s"[${System.currentTimeMillis()}] $to to $from reply $id: $v")
              FakeDelayer.executor.schedule(
                new Runnable() {
                  override def run(): Unit = {
                    if (LOGGING) println(s"[${System.currentTimeMillis()}] $from receive reply $id")
                    promise.complete(v)
                  }
                },
                fakeDelay.toMillis,
                TimeUnit.MILLISECONDS
              )
            }(FullMVUtil.notWorthToMoveToTaskpool)
          }
        },
        fakeDelay.toMillis,
        TimeUnit.MILLISECONDS
      )
      promise.future
    }
  }
}
