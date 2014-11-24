package rescala.propagation.turns.instances

import rescala.propagation.turns.Turn

object UnSynchronized extends AbstractTurnFactory[Synchronized](() => new Synchronized()) {
  override def newTurn[T](f: Turn => T): T = super.newTurn(f)
  override def lockingPhase(turn: Synchronized): Unit = ()
  override def realeasePhase(turn: Synchronized): Unit = ()
}
