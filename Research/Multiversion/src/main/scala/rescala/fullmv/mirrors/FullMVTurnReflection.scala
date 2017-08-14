package rescala.fullmv.mirrors

import java.util.concurrent.atomic.AtomicInteger

import rescala.fullmv.TurnPhase
import rescala.fullmv.TurnPhase.Type
import rescala.fullmv.{FullMVTurn, TransactionSpanningTreeNode}
import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.sgt.synchronization.SubsumableLock.GUID

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class FullMVTurnReflection(override val guid: SubsumableLock.GUID, val proxy: FullMVTurnMirrorProxy) extends FullMVTurn with FullMVTurnReflectionProxy {
  object phaseParking
  var phase: TurnPhase.Type = TurnPhase.Initialized
  object subLock
  @volatile var predecessors: Set[FullMVTurn] = Set.empty

  var localBranchCountBuffer = new AtomicInteger(0)

  var replicators: Set[FullMVTurnReflectionProxy] = Set.empty

  override def awaitPhase(atLeast: TurnPhase.Type): Unit = phaseParking.synchronized {
    while(phase < atLeast) phaseParking.wait()
  }

  override def activeBranchDifferential(forState: TurnPhase.Type, differential: Int): Unit = {
    assert(phase == forState, s"$this received branch differential for wrong state $forState")
    assert(differential != 0, s"$this received 0 branch diff")
    assert(localBranchCountBuffer.get + differential >= 0, s"$this received branch diff into negative count")
    val remaining = localBranchCountBuffer.addAndGet(differential)
    if(remaining == 0) {
      proxy.asyncRemoteBranchComplete(forState)
    }
  }

  override def asyncRemoteBranchComplete(forPhase: Type): Unit = activeBranchDifferential(forPhase, -1)

  override def isTransitivePredecessor(txn: FullMVTurn): Boolean = predecessors(txn)

  override def addReplicator(replicator: FullMVTurnReflectionProxy): (TurnPhase.Type, Set[FullMVTurn]) = subLock.synchronized {
    replicators += replicator
    (phase, predecessors)
  }

  override def removeReplicator(replicator: FullMVTurnReflectionProxy): Unit = subLock.synchronized {
    replicators -= replicator
  }

  override def newPredecessors(predecessors: Iterable[FullMVTurn]): Future[Unit] = {
    val reps = subLock.synchronized {
      this.predecessors ++= predecessors
      Future.successful(Unit)
      replicators
    }
    val forwards = reps.map(_.newPredecessors(predecessors))
    for(call <- forwards) {
      Await.result(call, Duration.Zero) // TODO Duration.Inf
    }
    Future.successful(Unit)
  }

  override def newPhase(phase: TurnPhase.Type): Future[Unit] = {
    val reps = subLock.synchronized {
      phaseParking.synchronized {
        if (this.phase < phase) {
          this.phase = phase
          phaseParking.notifyAll()
        }
      }
      replicators
    }
    val forwards = reps.map(_.newPhase(phase))
    for(call <- forwards) {
      Await.result(call, Duration.Zero) // TODO Duration.Inf
    }
    Future.successful(Unit)
  }

  override def acquirePhaseLockAndGetEstablishmentBundle(): Future[(TurnPhase.Type, TransactionSpanningTreeNode[FullMVTurn])] = proxy.acquirePhaseLockAndGetEstablishmentBundle()
  override def blockingAddPredecessorAndReleasePhaseLock(predecessorSpanningTree: TransactionSpanningTreeNode[FullMVTurn]): Unit = proxy.blockingAddPredecessorAndReleasePhaseLock(predecessorSpanningTree)
  override def asyncReleasePhaseLock(): Unit = proxy.asyncReleasePhaseLock()
  override def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = proxy.maybeNewReachableSubtree(attachBelow, spanningSubTreeRoot)

  override def newSuccessor(successor: FullMVTurn): Future[Unit] = proxy.newSuccessor(successor)
  override def getLockedRoot: Option[GUID] = proxy.getLockedRoot
  override def tryLock(): SubsumableLock.TryLockResult = proxy.tryLock()
  override def lock(): SubsumableLock.TryLockResult = proxy.lock()
  override def spinOnce(backoff: Long): SubsumableLock.TryLockResult = proxy.spinOnce(backoff)
  override def trySubsume(lockedNewParent: SubsumableLock.TryLockResult): Option[SubsumableLock] = proxy.trySubsume(lockedNewParent)
}
