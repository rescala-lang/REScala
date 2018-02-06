package rescala.fullmv.mirrors

import rescala.fullmv.{FullMVTurn, TransactionSpanningTreeNode, TurnPhase}

import scala.concurrent.Future

trait FullMVTurnPhaseReflectionProxy {
  def asyncNewPhase(phase: TurnPhase.Type): Unit
}

trait FullMVTurnPredecessorReflectionProxy {
  def newPredecessors(predecessors: TransactionSpanningTreeNode[FullMVTurn]): Future[Unit]
}
