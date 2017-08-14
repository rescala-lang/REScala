package rescala.fullmv.mirrors

import rescala.fullmv.TurnPhase.Type
import rescala.fullmv.{FullMVTurn, TransactionSpanningTreeNode}
import rescala.fullmv.sgt.synchronization.SubsumableLock

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object FullMVTurnLocalClone {
  def localCloneSpanningTree(node: TransactionSpanningTreeNode[FullMVTurn]): TransactionSpanningTreeNode[FullMVTurn] = {
    val clone = new TransactionSpanningTreeNode(node.txn)
    clone.children = node.children.map(localCloneSpanningTree)
    clone
  }

  def apply(turn: FullMVTurn): FullMVTurn = {
    val mirrorProxy: FullMVTurnMirrorProxy = new FullMVTurnMirrorProxy {
      override def blockingAddPredecessorAndReleasePhaseLock(predecessorSpanningTree: TransactionSpanningTreeNode[FullMVTurn]): Unit = {
        turn.blockingAddPredecessorAndReleasePhaseLock(localCloneSpanningTree(predecessorSpanningTree))
      }
      override def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit] = {
        turn.maybeNewReachableSubtree(FullMVTurnLocalClone(attachBelow), localCloneSpanningTree(spanningSubTreeRoot))
      }
      override def acquirePhaseLockAndGetEstablishmentBundle(): Future[(Type, TransactionSpanningTreeNode[FullMVTurn])] = {
        val (phase, spanningTree) = Await.result(turn.acquirePhaseLockAndGetEstablishmentBundle(), Duration.Zero)
        Future.successful(phase -> localCloneSpanningTree(spanningTree))
      }
      override def asyncRemoteBranchComplete(forPhase: Type): Unit = turn.asyncRemoteBranchComplete(forPhase)
      override def newSuccessor(successor: FullMVTurn): Future[Unit] = turn.newSuccessor(FullMVTurnLocalClone(successor))
      override def asyncReleasePhaseLock(): Unit = turn.asyncReleasePhaseLock()
      override def spinOnce(backoff: Long): SubsumableLock.TryLockResult = SubsumableLockLocalClone.localCloneTryLockResult(turn.spinOnce(backoff))
      override def trySubsume(lockedNewParent: SubsumableLock.TryLockResult): Option[SubsumableLock] = {
        turn.trySubsume(SubsumableLockLocalClone.localCloneTryLockResult(lockedNewParent)).map(SubsumableLockLocalClone.apply)
      }
      override def tryLock(): SubsumableLock.TryLockResult = SubsumableLockLocalClone.localCloneTryLockResult(turn.tryLock())
      override def lock(): SubsumableLock.TryLockResult = SubsumableLockLocalClone.localCloneTryLockResult(turn.lock())
      override def getLockedRoot: Option[SubsumableLock.GUID] = turn.getLockedRoot
    }

    val reflection = new FullMVTurnReflection(turn.guid, mirrorProxy)

    val reflectionProxy: FullMVTurnReflectionProxy = new FullMVTurnReflectionProxy {
      override def newPhase(phase: Type): Future[Unit] = reflection.newPhase(phase)
      override def newPredecessors(predecessors: Iterable[FullMVTurn]): Future[Unit] = reflection.newPredecessors(predecessors.map(FullMVTurnLocalClone.apply))
    }

    val (initPhase, initPreds) = turn.addReplicator(reflectionProxy)
    reflection.newPhase(initPhase)
    reflection.newPredecessors(initPreds)

    reflection
  }
}
