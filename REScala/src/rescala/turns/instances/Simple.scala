package rescala.turns.instances

import rescala.graph.Reactive
import rescala.propagation.AbstractTurn

/** doing nothing can be useful on its own */
class Simple extends AbstractTurn {
  override def acquireDynamic(reactive: Reactive): Unit = ()
  override def lockingPhase(): Unit = initialValues.foreach(_())
  override def realeasePhase(): Unit = ()
}

