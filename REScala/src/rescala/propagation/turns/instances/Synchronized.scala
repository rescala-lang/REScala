package rescala.propagation.turns.instances

import rescala.propagation.Reactive
import rescala.propagation.turns.Turn

object Synchronized extends AbstractTurnFactory[Synchronized](() => new Synchronized()) {
  override def newTurn[T](f: Turn => T): T = synchronized(super.newTurn(f))
  override def acquirePreTurnLocks(turn: Synchronized): Unit = ()
  override def releaseAllLocks(turn: Synchronized): Unit = ()
}

class Synchronized extends AbstractTurn {

  def create[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
    val reactive = f
    dependencies.foreach(register(reactive))
    ensureLevel(reactive, dependencies)
    reactive
  }

  def createDynamic[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
    val reactive = f
    ensureLevel(reactive, dependencies)
    evaluate(reactive)
    reactive
  }

  /** nothing to do, everything is locked anyways */
  override def acquireDynamic(reactive: Reactive): Unit = ()
}

