package rescala.fullmv.mirrors

import rescala.fullmv.{FullMVTurn, TransactionSpanningTreeNode, TurnPhase}

import scala.concurrent.Future

trait FullMVTurnReflectionProxy {
  def newPredecessors(predecessors: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit]
  def newPhase(phase: TurnPhase.Type): Future[Unit]
}
