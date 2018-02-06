package rescala.fullmv.mirrors

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import rescala.fullmv._
import rescala.fullmv.TurnPhase.Type
import rescala.fullmv.mirrors.Host.GUID
import rescala.fullmv.sgt.synchronization.{SubsumableLock, SubsumableLockEntryPoint, TryLockResult, TrySubsumeResult}

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}

class FullMVTurnReflection(override val host: FullMVEngine, override val guid: Host.GUID, initialPhase: TurnPhase.Type, val proxy: FullMVTurnProxy) extends FullMVTurn with SubsumableLockEntryPoint with FullMVTurnPhaseReflectionProxy with FullMVTurnPredecessorReflectionProxy {
  val _phase: AtomicInteger = new AtomicInteger(initialPhase)
  override def phase: TurnPhase.Type = _phase.get

  val predecessorIndex: AtomicReference[(Set[FullMVTurn], TransactionSpanningTreeNode[FullMVTurn])] = new AtomicReference(null)
  override def selfNode: TransactionSpanningTreeNode[FullMVTurn] = {
    val maybeInitialized = predecessorIndex.get
    if(maybeInitialized == null) null else maybeInitialized._2
  }
  override def isTransitivePredecessor(txn: FullMVTurn): Boolean = (txn == this) || {
    val maybeInitialized = predecessorIndex.get
    maybeInitialized != null && maybeInitialized._1(txn)
  }

  var localBranchCountBuffer = new AtomicInteger(0)

  @volatile var predecessorSubscribed = -1
  object predecessorSubscriptionParking

  override def ensurePredecessorReplication(): Unit = {
    if(predecessorSubscribed != 1) {
      // ensure that only the first guy to call this actually registers a subscription
      // and all the others only continue after that first guy is done
      val mustSubscribe = predecessorSubscriptionParking.synchronized {
        if (predecessorSubscribed == -1) {
          predecessorSubscribed = 0
          true
        } else {
          while (predecessorSubscribed == 0) predecessorSubscriptionParking.wait()
          false
        }
      }
      if (mustSubscribe) {
        if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this subscribing predecessors.")
        newPredecessors(Await.result(proxy.addPredecessorReplicator(this), host.timeout))
        predecessorSubscriptionParking.synchronized {
          predecessorSubscribed = 1
          predecessorSubscriptionParking.notifyAll()
        }
      }
    }
  }

  override def activeBranchDifferential(forState: TurnPhase.Type, differential: Int): Unit = {
    assert(phase == forState, s"$this received branch differential for wrong state ${TurnPhase.toString(forState)}")
    assert(differential != 0, s"$this received 0 branch diff")
    assert(localBranchCountBuffer.get + differential >= 0, s"$this received branch diff into negative count")
    val before = localBranchCountBuffer.getAndAdd(differential)
    val after = before + differential
    if(before == 0) {
      if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this reactivated locally, registering remote branch.")
      Await.result(proxy.addRemoteBranch(forState), host.timeout)
    } else if(after == 0) {
      if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this done locally, deregistering remote branch.")
      proxy.asyncRemoteBranchComplete(forState)
    }
  }

  override def newBranchFromRemote(forState: TurnPhase.Type): Unit = {
    assert(phase == forState, s"$this received branch differential for wrong state ${TurnPhase.toString(forState)}")
    if(localBranchCountBuffer.getAndIncrement() != 0) {
      if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this received remote branch but still active; deregistering immediately.")
      proxy.asyncRemoteBranchComplete(forState)
    } else {
      if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this (re-)activated by remote branch.")
    }
  }

  override def newPredecessors(predecessors: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = {
    //  override def asyncNewPredecessors(predecessors: TransactionSpanningTreeNode[FullMVTurn]): Unit = {
    if(phase < TurnPhase.Completed) {
      val newPreds = FullMVTurnReflection.buildIndex(predecessors)
      @inline @tailrec def retryCommitWhileNewer(): Future[Unit] = {
        val before = predecessorIndex.get
        if ((before == null || before._1.size < newPreds.size) && phase < TurnPhase.Completed) {
          if (!predecessorIndex.compareAndSet(before, (newPreds, predecessors))) {
            retryCommitWhileNewer()
          } else {
            if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this updated predecessors: $newPreds.")
//            predecessorReplicators.get.foreach(_.asyncNewPredecessors(predecessors))
            FullMVEngine.broadcast(predecessorReplicators.get)(_.newPredecessors(predecessors))
          }
        } else {
          Future.unit
        }
      }
      retryCommitWhileNewer()
    } else {
      Future.unit
    }
  }

  @tailrec final override def asyncNewPhase(phase: TurnPhase.Type): Unit = {
    val currentPhase = _phase.get
    if(currentPhase < phase) {
      if(!_phase.compareAndSet(currentPhase, phase)) {
        asyncNewPhase(currentPhase)
      } else {
        phaseReplicators.get.foreach(_.asyncNewPhase(phase))
        if(phase == TurnPhase.Completed) {
          host.dropInstance(guid, this)
          phaseReplicators.set(null)
          predecessorReplicators.set(null)
          predecessorIndex.set(null)
        }
        wakeWaitersAfterPhaseSwitch(phase)
      }
    }
  }

  override def asyncRemoteBranchComplete(forPhase: Type): Unit = proxy.asyncRemoteBranchComplete(forPhase)
  override def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit] = proxy.addRemoteBranch(forPhase)

  override def acquirePhaseLockIfAtMost(maxPhase: Type): Future[TurnPhase.Type] = proxy.acquirePhaseLockIfAtMost(maxPhase).map{phase =>
    asyncNewPhase(phase)
    phase
  }(FullMVEngine.notWorthToMoveToTaskpool)
  override def addPredecessor(tree: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = proxy.addPredecessor(tree)
  override def asyncReleasePhaseLock(): Unit = proxy.asyncReleasePhaseLock()
  override def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = proxy.maybeNewReachableSubtree(attachBelow, spanningSubTreeRoot)

  override def newSuccessor(successor: FullMVTurn): Future[Unit] = proxy.newSuccessor(successor)

  override def getLockedRoot: Future[Option[GUID]] = proxy.getLockedRoot
  override def tryLock(): Future[TryLockResult] = {
    if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this sending tryLock request")
    proxy.remoteTryLock().map {res =>
      if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this received tryLock result $res (retaining remote transfer reference as thread reference)")
      res
    }(FullMVEngine.notWorthToMoveToTaskpool)
  }
  override def remoteTryLock(): Future[TryLockResult] = {
    if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this passing through tryLock request")
    proxy.remoteTryLock().map {res =>
      if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this passing through tryLock result $res (retaining remote transfer reference as thread reference)")
      res
    }(FullMVEngine.notWorthToMoveToTaskpool)
  }
  override def trySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult] = {
    if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this sending trySubsume $lockedNewParent request, adding remote parameter transfer reference")
    lockedNewParent.localAddRefs(1)
    proxy.remoteTrySubsume(lockedNewParent).map {res =>
      if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this received trySubsume $lockedNewParent result $res")
      res
    }(FullMVEngine.notWorthToMoveToTaskpool)
  }
  override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult] = {
    if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this passing through trySubsume $lockedNewParent request")
    proxy.remoteTrySubsume(lockedNewParent).map {res =>
      if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this passing through trySubsume $lockedNewParent result $res")
      res
    }(FullMVEngine.notWorthToMoveToTaskpool)
  }

  override def toString: String = s"FullMVTurnReflection($guid on $host, ${TurnPhase.toString(phase)}${if(localBranchCountBuffer.get != 0) s"(${localBranchCountBuffer.get})" else ""})"
}

object FullMVTurnReflection {
  def buildIndex[T](tree: TransactionSpanningTreeNode[T], accumulator: Set[T] = Set.empty[T]): Set[T] = {
    var accu = accumulator + tree.txn
    val it = tree.iterator()
    while(it.hasNext) accu = buildIndex(it.next, accu)
    accu
  }
}
