package rescala.fullmv.mirrors

import java.util.concurrent.atomic.AtomicInteger

import rescala.fullmv.{FullMVEngine, FullMVTurn, TransactionSpanningTreeNode, TurnPhase}
import rescala.fullmv.TurnPhase.Type
import rescala.fullmv.mirrors.Host.GUID
import rescala.fullmv.sgt.synchronization.{SubsumableLock, SubsumableLockEntryPoint, TrySubsumeResult}

import scala.concurrent.{Await, Future}

class FullMVTurnReflection(override val host: FullMVEngine, override val guid: Host.GUID, val proxy: FullMVTurnProxy) extends FullMVTurn with SubsumableLockEntryPoint with FullMVTurnReflectionProxy {
  object phaseParking
  var phase: TurnPhase.Type = TurnPhase.Initialized
  object subLock
  var selfNode: TransactionSpanningTreeNode[FullMVTurn] = _
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

  override def addReplicator(replicator: FullMVTurnReflectionProxy): (TurnPhase.Type, TransactionSpanningTreeNode[FullMVTurn]) = subLock.synchronized {
    replicators += replicator
    (phase, selfNode)
  }

  private def indexChildren(node: TransactionSpanningTreeNode[FullMVTurn]): Unit = {
    this.predecessors += node.txn
    val it = node.iterator()
    while(it.hasNext) indexChildren(it.next())
  }

  override def newPredecessors(predecessors: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = {
    val reps = subLock.synchronized {
      this.selfNode = predecessors
      indexChildren(selfNode)
      replicators
    }
    val forwards = reps.map(_.newPredecessors(predecessors))
    for(call <- forwards) {
      Await.result(call, host.timeout)
    }
    Future.successful(Unit)
  }

  override def newPhase(phase: TurnPhase.Type): Future[Unit] = {
    val reps = subLock.synchronized {
      if(this.phase < phase) {
        this.phase = phase
        phaseParking.synchronized {
          phaseParking.notifyAll()
        }
        if (phase == TurnPhase.Completed) {
          predecessors = Set.empty
          host.dropInstance(guid, this)
        }
        replicators
      } else {
        Set.empty
      }
    }
    val forwards = reps.map(_.newPhase(phase))
    for (call <- forwards) {
      Await.result(call, host.timeout)
    }
    Future.successful(Unit)
  }

  override def asyncRemoteBranchComplete(forPhase: Type): Unit = proxy.asyncRemoteBranchComplete(forPhase)
  override def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit] = proxy.addRemoteBranch(forPhase)

  override def acquirePhaseLockIfAtMost(maxPhase: Type): Future[TurnPhase.Type] = proxy.acquirePhaseLockIfAtMost(maxPhase)
  override def addPredecessor(tree: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = proxy.addPredecessor(tree)
  override def asyncReleasePhaseLock(): Unit = proxy.asyncReleasePhaseLock()
  override def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = proxy.maybeNewReachableSubtree(attachBelow, spanningSubTreeRoot)

  override def newSuccessor(successor: FullMVTurn): Future[Unit] = proxy.newSuccessor(successor)

  override def getLockedRoot: Future[Option[GUID]] = proxy.getLockedRoot
  override def lock(): Future[SubsumableLock] = proxy.lock()
  override def trySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult] = proxy.trySubsume(lockedNewParent)

  override def toString: String = s"FullMVTurnReflection($guid on $host, ${TurnPhase.toString(phase)}${if(localBranchCountBuffer.get != 0) s"(${localBranchCountBuffer.get})" else ""})"
}
