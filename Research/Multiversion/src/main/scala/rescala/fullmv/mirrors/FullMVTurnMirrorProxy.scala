package rescala.fullmv.mirrors

import rescala.fullmv.{FullMVTurn, TransactionSpanningTreeNode, TurnPhase}
import rescala.fullmv.sgt.synchronization.SubsumableLock

import scala.concurrent.Future

trait FullMVTurnMirrorProxy {
  def getLockedRoot: Option[Host.GUID]
  def tryLock(): SubsumableLock.TryLockResult
  def lock(): SubsumableLock.TryLockResult
  def spinOnce(backoff: Long): SubsumableLock.TryLockResult
  def trySubsume(lockedNewParent: SubsumableLock.TryLockResult): Option[SubsumableLock]

  def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit

  def acquirePhaseLockAndGetEstablishmentBundle(): Future[(TurnPhase.Type, TransactionSpanningTreeNode[FullMVTurn])]
  def blockingAddPredecessorAndReleasePhaseLock(predecessorSpanningTree: TransactionSpanningTreeNode[FullMVTurn]): Unit
  def asyncReleasePhaseLock(): Unit

  def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit]
  def newSuccessor(successor: FullMVTurn): Future[Unit]
}
