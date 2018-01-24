package rescala.fullmv.mirrors

import rescala.fullmv.sgt.synchronization.{SubsumableLock, TryLockResult, TrySubsumeResult}
import rescala.fullmv.{FullMVTurn, TransactionSpanningTreeNode, TurnPhase}

import scala.concurrent.Future

trait FullMVTurnProxy {
  def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit]
  def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit

  def acquirePhaseLockIfAtMost(maxPhase: TurnPhase.Type): Future[TurnPhase.Type]
  def addPredecessor(tree: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit]
  def asyncReleasePhaseLock(): Unit

  def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit]
  def newSuccessor(successor: FullMVTurn): Future[Unit]

  def getLockedRoot: Future[Option[Host.GUID]]
  def remoteTryLock(): Future[TryLockResult]
  def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult]
}
