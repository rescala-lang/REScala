package rescala.fullmv.mirrors

import rescala.fullmv.{FullMVTurn, TransactionSpanningTreeNode, TurnPhase}
import rescala.fullmv.sgt.synchronization.SubsumableLockEntryPoints

import scala.concurrent.Future

trait FullMVTurnMirrorProxy extends SubsumableLockEntryPoints {
  def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit

  def acquirePhaseLockAndGetEstablishmentBundle(): Future[(TurnPhase.Type, TransactionSpanningTreeNode[FullMVTurn])]
  def blockingAddPredecessorAndReleasePhaseLock(predecessorSpanningTree: TransactionSpanningTreeNode[FullMVTurn]): Unit
  def asyncReleasePhaseLock(): Unit

  def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit]
  def newSuccessor(successor: FullMVTurn): Future[Unit]
}
