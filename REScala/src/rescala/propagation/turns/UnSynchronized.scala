package rescala.propagation.turns

import rescala.propagation.Turn

object UnSynchronized extends AbstractTurnFactory[Synchronized](() => new Synchronized()) {
  override def newTurn[T](f: Turn => T): T = super.newTurn(f)
  override def acquirePreTurnLocks(turn: Synchronized): Unit = ()
  override def releaseAllLocks(turn: Synchronized): Unit = ()
}
