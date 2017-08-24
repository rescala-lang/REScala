package rescala.fullmv.mirrors

import rescala.fullmv.TurnPhase

import scala.concurrent.Future

trait FullMVTurnReflectionProxy {
  def newPredecessors(predecessors: Seq[Host.GUID]): Future[Unit]
  def newPhase(phase: TurnPhase.Type): Future[Unit]
}
