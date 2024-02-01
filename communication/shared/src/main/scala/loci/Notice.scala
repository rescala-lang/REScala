package loci

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}

import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent.duration.Duration
import scala.concurrent.{Awaitable, CanAwait, Future, Promise, TimeoutException}
import scala.util.Try
import scala.util.control.NonFatal

sealed trait Notice[-T] extends (T => Unit) {
  def remove(): Unit
}

object Notice {
  type FailureReporter = Throwable => Unit

  sealed trait Consumer[+C[+U] <: Consumer[C, U], +T] {
    private[Notice] val notices: ConcurrentLinkedQueue[T => _] @uncheckedVariance =
      new ConcurrentLinkedQueue[T => _]

    private[Notice] def apply(v: T @uncheckedVariance): Boolean

    private[Notice] def create[U](failureReporter: FailureReporter): C[U]

    def failureReporter: FailureReporter

    def monitor[U, R](notice: T => R)(implicit ev: notice.type <:< (U => R)): Notice[U] =
      foreach(notice)(ev)

    def foreach[U, R](notice: T => R)(implicit ev: notice.type <:< (U => R)): Notice[U] = {
      notices.add(notice)
      new Notice[U] {
        def apply(v: U) = ev(notice)(v)
        def remove() = notices.remove(notice)
      }
    }
  }

  object Consumer {
    sealed trait TotalOperations[+C[+U] <: Consumer[C, U], +T] { this: Consumer[C, T] =>
      def map[U](function: T => U, failureReporter: FailureReporter = failureReporter): C[U] = {
        val notice = create[U](failureReporter)
        foreach { value => notice(function(value)) }
        notice
      }
    }

    sealed trait PartialOperations[+C[+U] <: Consumer[C, U], +T] { this: Consumer[C, T] =>
      def filter(predicate: T => Boolean, failureReporter: FailureReporter = failureReporter): C[T] = {
        val notice = create[T](failureReporter)
        foreach { value => if (predicate(value)) notice(value) }
        notice
      }

      def collect[U](function: PartialFunction[T, U], failureReporter: FailureReporter = failureReporter): C[U] = {
        val notice = create[U](failureReporter)
        val run = function runWith { notice(_) }
        foreach { run(_) }
        notice
      }
    }

    sealed trait SingleNotice[+C[+U] <: Consumer[C, U], +T] { this: Consumer[C, T] =>
      private[Notice] def apply(v: T @uncheckedVariance): Boolean = {
        while (!notices.isEmpty)
          try {
            val notice = notices.poll()
            if (notice != null)
              notice(v)
          }
          catch { case NonFatal(exception) => failureReporter(exception) }
        true
      }
    }

    sealed trait MultiNotice[+C[+U] <: Consumer[C, U], +T] { this: Consumer[C, T] =>
      private[Notice] def apply(v: T @uncheckedVariance): Boolean = {
        val iterator = notices.iterator
        while (iterator.hasNext)
          try iterator.next()(v)
          catch { case NonFatal(exception) => failureReporter(exception) }
        true
      }
    }
  }


  final class Stream[+T] private (val failureReporter: FailureReporter)
      extends Consumer[Stream, T]
      with Consumer.TotalOperations[Stream, T]
      with Consumer.PartialOperations[Stream, T]
      with Consumer.MultiNotice[Stream, T] {
    private[Notice] def create[U](failureReporter: FailureReporter) =
      new Stream[U](failureReporter)
  }

  object Stream {
    sealed class Source[-T] private[Stream] (notice: Stream[T]) {
      def failureReporter = notice.failureReporter
      def fire(v: T): Unit = notice(v)
      def fire()(implicit ev: Unit =:= T @uncheckedVariance): Unit = notice(())
    }

    final class NoticeSource[T] private[Stream] (val notice: Stream[T])
      extends Source[T](notice)

    def apply[T]: NoticeSource[T] =
      apply(logging.reportException)
    def apply[T](failureReporter: FailureReporter): NoticeSource[T] =
      new NoticeSource(new Stream(failureReporter))
  }


  final class Steady[+T] private (val failureReporter: FailureReporter)
      extends Consumer[Steady, T]
      with Consumer.TotalOperations[Steady, T]
      with Consumer.PartialOperations[Steady, T]
      with Consumer.SingleNotice[Steady, T]
      with Awaitable[T] {
    private[Notice] def create[U](failureReporter: FailureReporter) =
      new Steady[U](failureReporter)

    private val value: AtomicReference[Option[T]] @uncheckedVariance =
      new AtomicReference(Option.empty[T])

    def current: Option[T] = value.get

    override private[Notice] def apply(v: T @uncheckedVariance): Boolean =
      value.compareAndSet(None, Some(v)) && super.apply(v)

    override def foreach[U, R](notice: T => R)(implicit ev: notice.type <:< (U => R)): Notice[U] = {
      val result = super.foreach(notice)(ev)
      value.get foreach super.apply
      result
    }

    override def filter(predicate: T => Boolean, failureReporter: FailureReporter = failureReporter): Steady[T] = {
      val result = super.filter(predicate, failureReporter)
      value.get foreach { value => if (predicate(value)) result.value.set(Some(value)) }
      result
    }

    override def map[U](function: T => U, failureReporter: FailureReporter = failureReporter): Steady[U] = {
      val result = super.map(function, failureReporter)
      value.get foreach { value => result.value.set(Some(function(value))) }
      result
    }

    override def collect[U](function: PartialFunction[T, U], failureReporter: FailureReporter = failureReporter): Steady[U] = {
      val result = super.collect(function, failureReporter)
      value.get foreach (function runWith { value => result.value.set(Some(value)) })
      result
    }

    def toFutureFromTry[U](implicit ev: T <:< Try[U]): Future[U] = {
      val promise = Promise[U]()
      foreach { promise.complete(_) }
      promise.future
    }

    def toFuture: Future[T] = {
      val promise = Promise[T]()
      foreach { promise.success(_) }
      promise.future
    }

    @throws(classOf[TimeoutException])
    @throws(classOf[InterruptedException])
    def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
      if (atMost == Duration.Undefined)
        throw new IllegalArgumentException("cannot wait for undefined duration")

      if (current.isEmpty && atMost != Duration.MinusInf) {
        val latch = new CountDownLatch(1)
        foreach { _ => latch.countDown() }

        if (atMost == Duration.Inf)
          latch.await()
        else
          latch.await(atMost.toNanos, TimeUnit.NANOSECONDS)
      }

      if (current.isEmpty)
        throw new TimeoutException(s"steady notice timed out after $atMost")

      this
    }

    @throws(classOf[Exception])
    def result(atMost: Duration)(implicit permit: CanAwait): T =
      ready(atMost).value.get.get
  }

  object Steady {
    sealed class Source[-T] private[Steady] (notice: Steady[T]) {
      def failureReporter = notice.failureReporter
      def trySet(v: T): Boolean = notice(v)
      def trySet()(implicit ev: Unit =:= T @uncheckedVariance): Boolean = notice(())
      def set(v: T): Unit =
        if (!notice(v))
          throw new IllegalStateException("notice already steady")
      def set()(implicit ev: Unit =:= T @uncheckedVariance): Unit = set(())
    }

    final class NoticeSource[T] private[Steady] (val notice: Steady[T])
      extends Source[T](notice)

    def apply[T]: NoticeSource[T] =
      apply(logging.reportException)
    def apply[T](failureReporter: FailureReporter): NoticeSource[T] =
      new NoticeSource(new Steady(failureReporter))
  }


  final class Varying[+T] private (val failureReporter: FailureReporter)
      extends Consumer[Varying, T]
      with Consumer.TotalOperations[Varying, T]
      with Consumer.MultiNotice[Varying, T] {
    private[Notice] def create[U](failureReporter: FailureReporter) =
      new Varying[U](failureReporter)

    @volatile private var value: T @uncheckedVariance = _

    def current: T = value

    override private[Notice] def apply(v: T @uncheckedVariance): Boolean =
      if (value != v) {
        value = v
        super.apply(v)
      }
      else
        false

    override def foreach[U, R](notice: T => R)(implicit ev: notice.type <:< (U => R)): Notice[U] = {
      val result = super.foreach(notice)(ev)
      notice(value)
      result
    }

    override def map[U](function: T => U, failureReporter: FailureReporter = failureReporter): Varying[U] = {
      val result = super.map(function, failureReporter)

      var changed = true
      while (changed) {
        val current = value
        result.value = function(value)
        changed = current != value
      }

      result
    }
  }

  object Varying {
    sealed class Source[-T] private[Varying] (notice: Varying[T], init: T){
      def failureReporter = notice.failureReporter
      def set(v: T): Unit = notice(v)
      def set()(implicit ev: Unit =:= T @uncheckedVariance): Unit = notice(())
      notice(init)
    }

    final class NoticeSource[T] private[Varying] (val notice: Varying[T], init: T)
      extends Source[T](notice, init)

    def apply[T](init: T): NoticeSource[T] =
      apply(init, logging.reportException)
    def apply[T](init: T, failureReporter: FailureReporter): NoticeSource[T] =
      new NoticeSource(new Varying(failureReporter), init)
  }
}
