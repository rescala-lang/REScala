package rescala.core

import rescala.core.Initializer.{InitValues, Unchange}
import rescala.incremental.Delta

/** @author gerizuna
  * @since 04.07.19
  */
object CollectionsInitializer {

  class ReactiveEmptyDeltaUnchange[T] extends Unchange[Delta[T]] {
    override def unchange(v: Delta[T]): Delta[T] = Delta.noChange
  }

  def ReactiveEmptyDelta[T] = new InitValues[Delta[T]](Delta.noChange, new ReactiveEmptyDeltaUnchange[T])
}
