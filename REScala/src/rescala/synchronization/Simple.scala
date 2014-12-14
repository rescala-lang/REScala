package rescala.synchronization

import rescala.propagation.AbstractTurn

/** doing nothing can be useful on its own */
class Simple extends AbstractTurn {
  override def lockingPhase(): Unit = initialValues.foreach(_())
  override def realeasePhase(): Unit = ()
}

