package rescala.fullmv.mirrors

import rescala.fullmv.{FullMVTurn, TransactionSpanningTreeNode, TurnPhase}

import scala.concurrent.Future

trait FullMVTurnProxy extends SubsumableLockProxy {
  def addRemoteBranch(forPhase: TurnPhase.Type): Future[Unit]
  def asyncRemoteBranchComplete(forPhase: TurnPhase.Type): Unit

  def acquirePhaseLockAndGetEstablishmentBundle(): Future[(TurnPhase.Type, TransactionSpanningTreeNode[FullMVTurn])]
  def addPredecessorAndReleasePhaseLock(predecessorSpanningTree: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit]
  def asyncReleasePhaseLock(): Unit

  def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit]
  def newSuccessor(successor: FullMVTurn): Future[Unit]
}
