package rescala.fullmv.mirrors.localcloning

import java.util.concurrent.{ScheduledThreadPoolExecutor, TimeUnit}

import rescala.fullmv.FullMVEngine

import scala.concurrent.duration.Duration
import scala.concurrent.{Future, Promise}

object FakeDelayer {
  val executor = new ScheduledThreadPoolExecutor(1)

  def async(fakeDelay: Duration, op: => Unit): Unit = {
    if(fakeDelay == Duration.Zero) {
      op
    } else {
      FakeDelayer.executor.schedule(new Runnable() {
        override def run(): Unit = op
      }, fakeDelay.toMillis, TimeUnit.MILLISECONDS)
    }
  }

  def future[V](fakeDelay: Duration, future: Future[V]): Future[V] = {
    if(fakeDelay == Duration.Zero) {
      future
    } else {
      val promise = Promise[V]
      future.onComplete{ v =>
        async(fakeDelay, promise.complete(v))
      }(FullMVEngine.notWorthToMoveToTaskpool)
      promise.future
    }
  }
}
