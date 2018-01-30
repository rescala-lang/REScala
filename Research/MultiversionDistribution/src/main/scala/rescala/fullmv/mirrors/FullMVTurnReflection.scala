package rescala.fullmv.mirrors

import java.util.concurrent.atomic.AtomicInteger

import rescala.fullmv._
import rescala.fullmv.TurnPhase.Type
import rescala.fullmv.mirrors.Host.GUID
import rescala.fullmv.sgt.synchronization.{SubsumableLock, SubsumableLockEntryPoint, TryLockResult, TrySubsumeResult}

import scala.concurrent.{Await, Future}

class FullMVTurnReflection(override val host: FullMVEngine, override val guid: Host.GUID, initialPhase: TurnPhase.Type, val proxy: FullMVTurnProxy) extends FullMVTurn with SubsumableLockEntryPoint with FullMVTurnReflectionProxy {
  object phaseParking
  @volatile var phase: TurnPhase.Type = initialPhase
  object replicatorLock
  var _selfNode: Option[TransactionSpanningTreeNode[FullMVTurn]] = None
  def selfNode: TransactionSpanningTreeNode[FullMVTurn] = _selfNode.get
  @volatile var predecessors: Set[FullMVTurn] = Set()

  var localBranchCountBuffer = new AtomicInteger(0)

  var replicators: Set[FullMVTurnReflectionProxy] = Set.empty

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

  override def isTransitivePredecessor(txn: FullMVTurn): Boolean = txn == this || predecessors(txn)

  override def addReplicator(replicator: FullMVTurnReflectionProxy): (TurnPhase.Type, TransactionSpanningTreeNode[FullMVTurn]) = replicatorLock.synchronized {
    replicators += replicator
    (phase, selfNode)
  }

  private def indexChildren(predecessors: Set[FullMVTurn], node: TransactionSpanningTreeNode[FullMVTurn]): Set[FullMVTurn] = {
    val txn = node.txn
    var maybeChangedPreds = predecessors
    if(!predecessors.contains(txn)) maybeChangedPreds += txn
    val it = node.iterator()
    while(it.hasNext) maybeChangedPreds = indexChildren(maybeChangedPreds, it.next())
    maybeChangedPreds
  }

  override def newPredecessors(tree: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = synchronized {
    val newPreds = indexChildren(predecessors, _selfNode.getOrElse(CaseClassTransactionSpanningTreeNode[FullMVTurn](this, Array.empty[CaseClassTransactionSpanningTreeNode[FullMVTurn]])))
    if(newPreds ne predecessors) {
      predecessors = newPreds
      val reps = replicatorLock.synchronized {
        this._selfNode = Some(tree)
        replicators
      }
      FullMVEngine.broadcast(reps) { _.newPredecessors(tree) }
    } else {
      Future.unit
    }
  }

  override def newPhase(phase: TurnPhase.Type): Future[Unit] = synchronized {
    replicatorLock.synchronized {
      if(this.phase < phase) {
        this.phase = phase
        Some(replicators)
      } else None
    } match {
      case Some(reps) =>
        if (phase == TurnPhase.Completed) {
          predecessors = Set.empty
          host.dropInstance(guid, this)
        }
        FullMVEngine.broadcast(reps) { _.newPhase(phase) }
      case None =>
        Future.unit
    }
  }

  override def asyncRemoteBranchComplete(forPhase: Type): Unit = proxy.asyncRemoteBranchComplete(forPhase)
  override def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit] = proxy.addRemoteBranch(forPhase)

  override def acquirePhaseLockIfAtMost(maxPhase: Type): Future[TurnPhase.Type] = proxy.acquirePhaseLockIfAtMost(maxPhase)
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
