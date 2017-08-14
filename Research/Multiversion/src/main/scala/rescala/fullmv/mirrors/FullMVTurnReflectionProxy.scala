package rescala.fullmv.mirrors

import rescala.fullmv.{FullMVTurn, TurnPhase}

import scala.concurrent.Future

trait FullMVTurnReflectionProxy {
  def newPredecessors(predecessors: Iterable[FullMVTurn]): Future[Unit]
  def newPhase(phase: TurnPhase.Type): Future[Unit]
}
