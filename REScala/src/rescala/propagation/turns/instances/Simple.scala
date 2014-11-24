package rescala.propagation.turns.instances

import rescala.propagation.Reactive

/** doing nothing can be useful on its own */
class Simple extends AbstractTurn {
  override def acquireDynamic(reactive: Reactive): Unit = ()
  override def lockingPhase(): Unit = ()
  override def realeasePhase(): Unit = ()
}

