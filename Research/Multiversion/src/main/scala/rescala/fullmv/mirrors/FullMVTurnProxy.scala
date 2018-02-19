package rescala.fullmv.mirrors

import rescala.fullmv.sgt.synchronization.{LockStateResult, SubsumableLock, TryLockResult, TrySubsumeResult}
import rescala.fullmv.{FullMVTurn, TransactionSpanningTreeNode, TurnPhase}

import scala.concurrent.Future

trait FullMVTurnProxy {
  def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit]
  def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit

  def acquirePhaseLockIfAtMost(maxPhase: TurnPhase.Type): Future[TurnPhase.Type]
  def addPredecessor(tree: TransactionSpanningTreeNode[FullMVTurn]): Future[Boolean]
  def asyncReleasePhaseLock(): Unit

  def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit]
  def newSuccessor(successor: FullMVTurn): Future[Unit]

  def getLockedRoot: Future[LockStateResult]
  // result has one thread reference counted
  def remoteTryLock(): Future[TryLockResult]
  // parameter has one thread reference counted, result has one thread reference counted
  def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult]

  def asyncAddPhaseReplicator(replicator: FullMVTurnPhaseReflectionProxy, knownPhase: TurnPhase.Type): Unit
  def addPredecessorReplicator(replicator: FullMVTurnPredecessorReflectionProxy): Future[TransactionSpanningTreeNode[FullMVTurn]]
}
