package rescala.propagation.turns.instances

import rescala.propagation.Reactive

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
  override def lockingPhase(): Unit = ()
  override def realeasePhase(): Unit = ()
}

