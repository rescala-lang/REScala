package rescala.fullmv.mirrors

import rescala.fullmv.sgt.synchronization.SubsumableLockEntryPoint
import rescala.fullmv.{FullMVTurn, TransactionSpanningTreeNode, TurnPhase}

import scala.concurrent.Future

trait FullMVTurnProxy extends SubsumableLockEntryPoint {
  def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit]
  def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit

  def acquirePhaseLockIfAtMost(maxPhase: TurnPhase.Type): Future[TurnPhase.Type]
  def addPredecessor(tree: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit]
  def asyncReleasePhaseLock(): Unit

  def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit]
  def newSuccessor(successor: FullMVTurn): Future[Unit]
}
