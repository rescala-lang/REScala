package rescala.fullmv

import java.util.concurrent.ForkJoinPool

import rescala.core.SchedulerImpl
import rescala.fullmv.mirrors.{FullMVTurnHost, Host, HostImpl, SubsumableLockHostImpl}
import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.tasks.{Framing, SourceNotification}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.Try

class FullMVEngine(val timeout: Duration, val name: String) extends SchedulerImpl[FullMVStruct, FullMVTurn] with FullMVTurnHost with HostImpl[FullMVTurn] {
  override object lockHost extends SubsumableLockHostImpl {
    override def toString: String = "Locks " + name
  }
  override val dummy: FullMVTurnImpl = {
    val dummy = new FullMVTurnImpl(this, Host.dummyGuid, null, lockHost.newLock())
    instances.put(Host.dummyGuid, dummy)
    dummy.beginExecuting()
    dummy.completeExecuting()
    if(Host.DEBUG || SubsumableLock.DEBUG || FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this SETUP COMPLETE")
    dummy
  }
  def newTurn(): FullMVTurnImpl = createLocal(new FullMVTurnImpl(this, _, Thread.currentThread(), lockHost.newLock()))

  val threadPool = new ForkJoinPool() with ExecutionContext {
    override def reportFailure(cause: Throwable): Unit = cause.printStackTrace()
  }

  override private[rescala] def singleReadValueOnce[A](reactive: Signal[A]) = reactive.state.latestValue.get

  override def executeTurn[R](declaredWrites: Set[ReSource], admissionPhase: (AdmissionTicket) => R): R = {
    val turn = newTurn()
    withTurn(turn) {
      if (declaredWrites.nonEmpty) {
        // framing phase
        turn.beginFraming()
        turn.activeBranchDifferential(TurnPhase.Framing, declaredWrites.size)
        for (i <- declaredWrites) threadPool.submit(new Framing(turn, i))
        turn.completeFraming()
      } else {
        turn.beginExecuting()
      }

      // admission phase
      val admissionTicket = new AdmissionTicket(turn, declaredWrites) {
        override def access[A](reactive: Signal[A]): reactive.Value = turn.dynamicBefore(reactive)
      }
      val admissionResult = Try { admissionPhase(admissionTicket) }
      if (FullMVEngine.DEBUG) admissionResult match {
        case scala.util.Failure(e) => e.printStackTrace()
        case _ =>
      }
      assert(turn.activeBranches.get == 0, s"Admission phase left ${turn.activeBranches.get()} tasks undone.")

      // propagation phase
      if (declaredWrites.nonEmpty) {
        turn.initialChanges = admissionTicket.initialChanges
        turn.activeBranchDifferential(TurnPhase.Executing, declaredWrites.size)
        for(write <- declaredWrites) threadPool.submit(new SourceNotification(turn, write, admissionResult.isSuccess && turn.initialChanges.contains(write)))
      }

      // turn completion
      turn.completeExecuting()

      // wrap-up "phase"
      val transactionResult = if(admissionTicket.wrapUp == null){
        admissionResult
      } else {
        val wrapUpTicket = new WrapUpTicket(){
          override def access(reactive: ReSource): reactive.Value = turn.dynamicAfter(reactive)
        }
        admissionResult.map { i =>
          // executed in map call so that exceptions in wrapUp make the transaction result a Failure
          admissionTicket.wrapUp(wrapUpTicket)
          i
        }
      }

      // result
      transactionResult.get
    }
  }

  override def toString: String = "Turns " + name
  def cacheStatus: String = s"${instances.size()} turn instances and ${lockHost.instances.size()} lock instances"
}

object FullMVEngine {
  val DEBUG = true

  val default = new FullMVEngine(10.seconds, "default")

  object notWorthToMoveToTaskpool extends ExecutionContext {
    override def execute(runnable: Runnable): Unit = runnable.run()
    override def reportFailure(t: Throwable): Unit = throw new IllegalStateException("problem in scala.concurrent internal callback", t)
  }

  def myAwait[T](future: Future[T], timeout: Duration): T = {
//    Await.result(future, timeout)
    if(!future.isCompleted) {
      val blocker = new java.util.concurrent.ForkJoinPool.ManagedBlocker {
        override def isReleasable: Boolean = future.isCompleted
        override def block(): Boolean = { Await.ready(future, timeout); true }
      }
      ForkJoinPool.managedBlock(blocker)
    }
    future.value.get.get
  }

  type CallAccumulator[T] = List[Future[T]]
  def newAccumulator(): CallAccumulator[Unit] = Nil
  def broadcast[C](collection: Iterable[C])(makeCall: C => Future[Unit]): Future[Unit] = {
    condenseCallResults(accumulateBroadcastFutures(newAccumulator(), collection) (makeCall))
  }
  def accumulateBroadcastFutures[T, C](accumulator: CallAccumulator[T], collection: Iterable[C])(makeCall: C => Future[T]): CallAccumulator[T] = {
    collection.foldLeft(accumulator){ (acc, elem) => accumulateFuture(acc, makeCall(elem)) }
  }
  def accumulateFuture[T](accumulator: CallAccumulator[T], call: Future[T]): CallAccumulator[T] = {
    if(!call.isCompleted || call.value.get.isFailure) {
      call :: accumulator
    } else {
      accumulator
    }
  }
  def condenseCallResults(accumulator: Iterable[Future[Unit]]): Future[Unit] = {
    // TODO this should collect exceptions..
    accumulator.foldLeft(Future.unit) { (fu, call) => fu.flatMap(_ => call)(notWorthToMoveToTaskpool) }
  }
}
