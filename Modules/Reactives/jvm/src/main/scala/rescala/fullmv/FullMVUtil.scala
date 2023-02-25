package rescala.fullmv

import rescala.fullmv.mirrors.Mirror
import rescala.fullmv.sgt.synchronization.SubsumableLockBundle
import rescala.fullmv.tasks.TaskBundle
import rescala.operator.Interface

import java.util.concurrent.ForkJoinPool
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class FullMVApi(val timeout: Duration, val schedulerName: String) extends Interface with FullMVBundle with Mirror
    with TurnImplBundle with TaskBundle with FullMvStateBundle with SubsumableLockBundle {
  override val scheduler: FullMVEngine = new FullMVEngine(timeout, schedulerName)
}

object FullMVUtil {
  val DEBUG = false

  object default extends FullMVApi(Duration.Zero, "FullMV-default-engine")

  object notWorthToMoveToTaskpool extends ExecutionContext {
    override def execute(runnable: Runnable): Unit =
      try {
        runnable.run()
      } catch {
        case t: Throwable => new Exception("Exception in future mapping", t).printStackTrace()
      }
    override def reportFailure(t: Throwable): Unit =
      throw new IllegalStateException("problem in scala.concurrent internal callback", t)
  }

  def myAwait[T](future: Future[T], timeout: Duration): T = {
//    Await.result(future, timeout)
    if (!future.isCompleted) {
      val blocker = new java.util.concurrent.ForkJoinPool.ManagedBlocker {
        override def isReleasable: Boolean = future.isCompleted
        override def block(): Boolean      = { Await.ready(future, timeout); true }
      }
      ForkJoinPool.managedBlock(blocker)
    }
    future.value.get.get
  }

  type CallAccumulator[T] = List[Future[T]]
  def newAccumulator(): CallAccumulator[Unit] = Nil
  def broadcast[C](collection: Iterable[C])(makeCall: C => Future[Unit]): Future[Unit] = {
    condenseCallResults(accumulateBroadcastFutures(newAccumulator(), collection)(makeCall))
  }
  def accumulateBroadcastFutures[T, C](
      accumulator: CallAccumulator[T],
      collection: Iterable[C]
  )(makeCall: C => Future[T]): CallAccumulator[T] = {
    collection.foldLeft(accumulator) { (acc, elem) => accumulateFuture(acc, makeCall(elem)) }
  }
  def accumulateFuture[T](accumulator: CallAccumulator[T], call: Future[T]): CallAccumulator[T] = {
    if (!call.isCompleted || call.value.get.isFailure) {
      call :: accumulator
    } else {
      accumulator
    }
  }
  def condenseCallResults(accumulator: Iterable[Future[Unit]]): Future[Unit] = {
    // TODO this should collect exceptions..
    accumulator.foldLeft(Future.successful(())) { (fu, call) => fu.flatMap(_ => call)(notWorthToMoveToTaskpool) }
  }
}
